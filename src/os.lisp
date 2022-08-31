;; Helper functions to interact with the OS or the outside world.

(in-package :hly-git-tools)

(defmacro sh (action &rest r)
  "Utility macro for shell actions for optional trace output.

Uses the project wide convention of showing shell tracing output if envvar
DEBUGSH is defined and non-empty.
"
  `(,(find-symbol (symbol-name action) 'sh)
    ,@r
    :show (uiop:getenvp "DEBUGSH")))

(defun sh/ss (cmd)
  "Like sh:run/ss (string-stripped), but don't swallow stderr.

Useful because I expect stderr to have useful info for the end user. No need to
hide it.
"
  (sh run cmd :output '(:string :stripped t)))

(defun sh/lines (cmd)
  "Like sh:run/lines, but don't swallow stderr.

Useful because I expect stderr to have useful info for the end user. No need to
hide it.
"
  (sh run cmd :output :lines))

(defmacro sh/no-err (&body spec)
  "Like the sh command, but don't raise a signal on error.

Return T on success, NIL on failure.
"
  ;; We're clearly getting into an ugly area of combinatorial explosion. Maybe a
  ;; good solution is using dynamic vars? I want to keep growing ad-hoc and ugly
  ;; for now, so the problem can be really clear and painful before I try to
  ;; solve it. Maybe this is sufficient, after all.
  `(->> (sh ,@spec :on-error nil)
        (nth-value 2)
        (equal 0)))

(defun sh/silently (cmd)
  "Run this command without stdout/err, return T on success, NIL on failure"
  (sh/no-err run/nil cmd))

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
