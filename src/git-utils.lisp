;; Copyright © 2022, 2023  Hraban Luyat
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

(defpackage #:git-hly/src/git-utils
  (:use #:arrow-macros
        #:cl
        #:git-hly/src/cmd
        #:git-hly/src/os)
  (:import-from #:trivia)
  (:import-from #:uiop)
  (:export #:current-branch))

(in-package #:git-hly/src/git-utils)

(defun current-branch ()
  "Get current branch name, if any."
  (trivia:match (sh/ss '(git rev-parse --abbrev-ref "HEAD"))
    ("HEAD" NIL)
    (x x)))
