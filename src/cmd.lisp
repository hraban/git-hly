;; Subcommand handling, arg parsing

(in-package :hly-git-tools)

(defparameter *cmds* (make-hash-table :test 'equal))

(defmacro define-cmd (name &rest decl)
  "Register this as both a function and a user command."
  `(setf (gethash (string-upcase (string ',name)) *cmds*)
         (defun ,name ,@decl)))

(defun cmd-names ()
  (mapcar 'car (alex:hash-table-alist *cmds*)))

(defun get-cmd (name)
  (gethash (string-upcase name) *cmds*))

(defun fmt-err-and-exit (fmt &rest args)
  (apply #'format *error-output* fmt args)
  (uiop:quit 1))

(defun usage-str ()
  (format NIL "Valid commands:

~{- ~A
~}

For in-depth help, pass --help to a subcommand.
"
          (-> (cmd-names) (sort #'string<))))

(defun no-such-cmd (name)
  (fmt-err-and-exit "No such command: ~A~%~%~A" name (usage-str)))

(defun help-args-p (args)
  (or (equal args '("-h"))
      (member "--help" args :test #'equal)))

(declaim (ftype (function ((or symbol string)) t) print-help))
(defun print-help (name)
  (-<> name
       (get-cmd <>)
       (or <> (no-such-cmd name))
       (documentation <> 'function)
       (format T "~A: ~A~&" name <>)))

(declaim (ftype (function ((or symbol string) list) function) cmd))
(defun cmd (name args)
  "Call the command with these args"
  (if (member name '("help" "-h" "--help"))
      (if args
          (print-help (first args))
          (format T "~A" (usage-str)))
      (if (help-args-p args)
          (print-help name)
          (-> name
              get-cmd
              (or (no-such-cmd name))
              (apply args)))))

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
