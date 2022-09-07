(in-package :hly-git-tools)

(defun current-branch ()
  "Get current branch name, if any."
  (match (sh/ss '(git rev-parse --abbrev-ref "HEAD"))
    ("HEAD" NIL)
    (x x)))

(defun remote-tracking (b)
  "Get remote tracking of given branch"
  ;; https://stackoverflow.com/a/9753364
  (let ((refname (format NIL "~A@{u}" b)))
    (sh/ss `(git rev-parse --abbrev-ref --symbolic-full-name ,refname))))

(defun git-pullfetch-aux ()
  "Smart pull & fetch as much as possible."
  (let ((master-b (sh/ss '(git config init.defaultBranch)))
        (current-b (current-branch)))
    (sh run '(and (git fetch --prune)
                  (git commit-graph write --append)))
    (if (string= current-b master-b)
        (sh run '(git merge --ff-only))
        (let ((master-r (remote-tracking master-b)))
          (sh run `(git branch --force ,master-b ,master-r))))))

(define-cmd git-pullfetch ()
  ;; If any of the subprocesses caused an error, just return that directly
  ;; without printing the entire lisp stack trace.
  (handler-case (git-pullfetch-aux)
    (uiop:subprocess-error (c)
      (uiop:quit (uiop:subprocess-error-code c)))))

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
