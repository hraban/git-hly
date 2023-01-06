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

(defpackage #:git-hly/src/cmds/children
  (:use #:arrow-macros
        #:cl
        #:git-hly/src/cmd
        #:git-hly/src/os)
  (:export #:get-child-branches))

(in-package #:git-hly/src/cmds/children)

(defun get-child-branches (parent)
  "Get all child branches for this parent ref.

If the parent is itself a branch, include it.
"
  (sh/lines `(git branch
                  --contains ,parent
                  --no-color
                  --format "%(refname:lstrip=2)")))

(define-cmd children (parent)
  "Get all children of this commit, without the commit itself.

Useful for scripting. Similar to:

    git branch --contains XXX | grep -v XXX

... sort of.
"
  ;; TODO: Option to filter only commits which I committed.
  (-<>> parent
        get-child-branches
        ;; TODO: should remove any branch which points to the same ref as
        ;; parent. That’s distinctly different from a string match.
        (remove parent <> :test #'equal)
        (format T "~{~A~%~}")))
