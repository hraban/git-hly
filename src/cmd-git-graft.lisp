(in-package :hly-git-tools)

(defun get-child-branches (parent)
  "Get all child branches for this parent ref.

If the parent is itself a branch, include it.
"
  (sh/lines `(git branch
                  --contains ,parent
                  --no-color
                  --format "%(refname:lstrip=2)")))

(defun ancestor-p (parent child)
  "T if parent is actually an ancestor of child"
  (sh/silently `(git merge-base --is-ancestor ,parent ,child)))

(defun do-tree (predicate parent cb l)
  "Iterate through a tree, represented as a pre-ordered list.

The predicate defines the ancestry relationship, *transitively*. E.g. < or ≤ or
⊆ or string-prefix-p. The list must be ordered by this predicate.

The domain of the predicate is a partial order; that’s why e.g. ⊂ and
string-prefix-p are valid predicates. That’s a fancy way of saying: it’s ok for
some elements to not be comparable at all, e.g. {x} ⊄ {y} /and/ {y} ⊄ {x}.

The callback is called on every direct parent-child pair in the tree. So for
1,2,3, you get cb(1,2) and cb(2,3) but not cb(1,3). Even though the predicate
should return T for P(1,3).

The base is a parent value which is used as the original root. The return value
is the list starting at the first element which is not a child of this parent.

E.g. if root is 0, ancestry is ≤, and list is '(0 1 1 0 1 -2 1 2), the return
value would be '(-2 1 2).

Normally, you'd call this with a parent element of all elements, and you expect
to get the empty list as a return value.

Example:

  (do-tree #'uiop:string-prefix-p
           \"\"
           (lambda (x y) (format T \"~A -> ~A~%\" x y))
           (uiop:split-string \"A AA AAA AAB AB AC B BA BB\"))

 -> A
A -> AA
AA -> AAA
AA -> AAB
A -> AB
A -> AC
 -> B
B -> BA
B -> BB

NIL
"
  (when l
    (destructuring-bind (head . rest) l
      (if (funcall predicate parent head)
          (progn
            ;; The head element is part of this ancestry.
            (funcall cb parent head)
            (->> ;; Consume the direct children of this node...
                 (do-tree predicate head cb rest)
                 ;; ... then, continue with its siblings
                 (do-tree predicate parent cb)))
          ;; We've exhausted this ancestry. Return the unprocessed elements.
          l))))

(defun rev-parse (ref)
  (sh/ss `(git rev-parse ,ref)))

(defun keep-ancestry (parent refs)
  (remove-if-not (lambda (r) (ancestor-p parent r)) refs))

(defun git-move (branch from onto)
  "Echo the operations necessary to rebase a branch to a new parent."
  (format T "git checkout \"~A\" && \\~%git rebase --onto \"~A\" \"~A\" && \\~%"
          branch
          onto
          from))

(defun print-undo-branch (b)
  "Print what it would take to reset this branch to its existing spot."
  (format T ">&2 echo 'git branch -f ~A ~A'~%" b (rev-parse b)))

(defun print-undo (bs)
  (format T ">&2 echo '# Run these commands to undo the entire operation:'~%")
  (mapc #'print-undo-branch bs)
  (format T ">&2 echo~%"))

(defun git-parent (ref)
  (format NIL "~A^" ref))

(defun print-push-all (branches)
  (format T ">&2 echo '# To push all to origin:
git push --force-with-lease origin ~{~S:~:*~S~^ ~}'~%" branches))

(define-cmd git-graft (root onto)
  "Print a chain of commands to move an entire subtree of git branches.

Usage:

  git-graft ROOT ONTO

Move all branches that are a child of one commit, onto another commit, while
maintaining the git commit tree structure.

ROOT: the first commit under (and including) which all local branches will be
selected.

ONTO: the commit onto which the subtree will be moved.

Example: say after a git fetch, you end up with:

  A -> B -> master -> C -> D -> origin/master
              \\
                --> E -> branch1 -> F -> branch2
                           \\
                             -----> G -> branch3

Execute this:

  git-graft E origin/master

And you will get a bunch of git commands on stdout. Execute them all, and you
should end up with:

  ... -> D -> origin/master -> E -> branch1 -> F -> branch2
                                      \\
                                        -----> G -> branch3

Every local branch under (and including) branch1 will be moved, so be careful
choosing something like (e.g.) master. It will work if all branches under master
are indeed feature branches that you 'own', but if you locally checked out
someone else's branch, it will also be rebased.

Git-graft also prints \"undo\" commands, commented out. These are not expected
to be necessary for normal operation, but if something goes wrong midway through
a big graft, you can use those commands to go back to the pre-graft state.

The argument to git-graft, the root, is *inclusive*. This is at odds with git
conventions, where you normally specify a commit's 'parent', under which all
relevant commits are found. The reason is that sometimes, e.g. the example
above, choosing the parent can lead to too many branches being included: you'd
get origin/master, and all potential other local branches already ported onto
it.
"
         ;; rev-parse right now to fix this commit, because if it's a branch
         ;; name it might change during this operation.
  (let* ((root (rev-parse root))
         ;; In git speak, a rebase is "exclusive" from the "from", iow it starts
         ;; at "from + 1", iow "only the from's children, not the from itself".
         (from (git-parent root)))
    (let ((branches (-> root
                        get-child-branches
                        (sort #'ancestor-p))))
      ;; Print all branch names to allow user to copy/paste into a git push
      ;; --force-with-lease origin ... line after all is done.
      (format T ">&2 echo '# rebasing branches: ~{~A~^ ~}'~%" branches)
      (print-undo branches)
      (format T "set -x~%")
      (do-tree
        #'ancestor-p
        from
        (lambda (base branch)
          ;; The root must be moved to the new destination. Everything else
          ;; must be moved to its "current" parent, because that is a branch
          ;; name, and that branch name will already have been moved to a
          ;; new destination. So a current branch’s parent branch name will
          ;; automatically become a new location by the time you're moving.
          (let ((onto (if (equal base from)
                          ;; By passing in the “onto” ref verbatim, if you try
                          ;; to graft onto HEAD, you’d be rebasing onto a
                          ;; different HEAD by the time the rebase command is
                          ;; actually executed, because you already checked out
                          ;; to a different branch. To avoid this, use rev-parse
                          ;; at command generation time, to make sure the
                          ;; revision doesn’t change out from under us.

                          (rev-parse onto)
                          base)))
            ;; Rev-parse the old base now, because the base branch will
            ;; move. Conversely: don't rev-parse the target base branch
            ;; because you want to rebase onto the new one. For most cases,
            ;; this is just (git-move branch (rev-parse base) base).
            (git-move branch (rev-parse base) onto)))
        branches)
      ;; The do-tree also prints a trailing "&&" for every command, to create a
      ;; long AND-chain of commands that only get executed if the last one
      ;; succeeded. This leaves one trailing && which must be followed by a
      ;; valid command. That’s what this ‘echo Done’ is really for. It must be a
      ;; command that can be safely ignored when there is failure, so e.g. set
      ;; +x can’t be it.
      (format T ">&2 echo '# Done.'~%")
      (format T "set +x~%")
      (print-push-all branches))))

;; Copyright © 2022  Hraban Luyat
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, version 3 of the License.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
