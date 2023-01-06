;; Copyright © 2023  Hraban Luyat
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

(defpackage #:git-hly/src/cmds/branch-rename
  (:use #:cl
        #:git-hly/src/cmd
        #:git-hly/src/git-utils
        #:git-hly/src/os)
  (:local-nicknames (#:sh #:inferior-shell)))

(in-package #:git-hly/src/cmds/branch-rename)

(defun rename (from to current-branch)
  (sh run `(and (git checkout --quiet "HEAD^{}")
                (git branch -f ,to ,from)
                (git branch -D ,from)
                ,(cond
                   ((equal current-branch from)
                    `(git checkout ,to))
                   (current-branch
                    ;; I was at least on /something/ we can switch back to
                    '(git switch -))
                   (t
                    ;; And if I wasn’t on a branch, leave it that way.
                    '(true))))))

(defun quit-err (msg &rest format-args)
  (apply #'format *error-output* msg format-args)
  (format *error-output* "~&")
  (uiop:quit 1))

(define-cmd branch-rename (from &optional to)
  "Rename a branch.

Usage:

    branch-rename [FROM] TO

When the from branch name is ommitted, default to the current branch.
"
  (let ((cur (current-branch)))
    (cond
      (to (rename from to cur))
      (cur (rename cur from cur))
      (t (quit-err "No FROM name passed, and not currently on a branch")))))
