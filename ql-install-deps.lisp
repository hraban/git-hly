#!/usr/bin/env -S sbcl --noinform --script

;; Install quicklisp dependencies of all your local systems. Doesn't actually
;; load the systems (only their .asd file), which is useful to create separate
;; Docker layer for dependencies and system itself.
;;
;; From:
;; https://github.com/quicklisp/quicklisp-client/issues/134

;; More portable way to get home dir's quicklisp
(load (merge-pathnames #p"quicklisp/setup.lisp" (user-homedir-pathname)))

(loop for x in (ql:list-local-systems)
      for y = (asdf:find-system x)
      append (asdf:system-depends-on y) into deps
      finally (ql:quickload deps))

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
