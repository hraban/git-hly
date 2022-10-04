(in-package :git-hly)

(defun main (&optional argv)
  "Argv follows buildapp convention: single list containing full arg"
  (let* ((argv (or argv (uiop:raw-command-line-arguments)))
         (bin (-> argv first file-namestring)))
    (if (string= bin "git-hly")
        (cmd (cadr argv) (cddr argv))
        (cmd bin (rest argv)))))

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
