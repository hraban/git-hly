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

(defpackage #:git-hly
  (:nicknames #:git-hly/main)
  (:use #:arrow-macros #:cl #:git-hly/src/cmd)
  (:import-from #:uiop)
  (:import-from #:trivia)
  (:import-from #:trivia.ppcre)
  (:import-from #:git-hly/src/cmds/*)
  (:export #:main))

(in-package #:git-hly/main)

(defun main (&optional argv)
  "Argv follows buildapp convention: single list containing full arg"
  (let* ((argv (or argv (uiop:raw-command-line-arguments))))
    (trivia:match (-> argv first file-namestring)
      ("git-hly"
       (cmd (cadr argv) (cddr argv)))
      (;; If the command name starts with “git-”, strip that prefix
       (trivia.ppcre:ppcre "^git-(.*)" name)
       (cmd name (rest argv)))
      (name
       (cmd name (rest argv))))))
