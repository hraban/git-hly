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

(defpackage #:git-hly/src/cmds/split
  (:use #:arrow-macros
        #:cl
        #:git-hly/src/cmd
        #:git-hly/src/os)
  (:local-nicknames (#:sh #:inferior-shell)))

(in-package #:git-hly/src/cmds/split)

(defun changed-files (ref)
  (sh/lines `(git diff-tree :no-commit-id :name-only #\r ,ref)))

(define-cmd split ()
  "Split the HEAD commit up into separate commits per file"
  (let ((rev (sh/ss '(git rev-parse "HEAD")))
        ;; Use git for this because the actual length depends on some heuristics
        ;; I don’t care to know
        (rev-short (sh/ss '(git rev-parse :short "HEAD"))))
    (sh run '(git reset "HEAD^"))
    (dolist (file (changed-files rev))
      (sh run `(git add -- ,file))
      (sh run `(sh:pipe (sh:progn (printf "%s: %s\\n\\n" ,rev-short ,file)
                                  (git show #\s "--format=%B" ,rev))
                        (git commit #\F -))))))
