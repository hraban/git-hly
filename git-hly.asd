;; Copyright © 2022–2024  Hraban Luyat
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

(asdf:defsystem "git-hly"
  :class :package-inferred-system
  :description "Various git utilities by Hraban"
  :version "0.1"
  :author "Hraban Luyat"
  :build-operation "program-op"
  :build-pathname "bin/git-hly"
  :entry-point "git-hly:main"
  ;; ECL wants an explicit note that ASDF is required, and it must be first in
  ;; the depends-on list (!)
  :depends-on ("asdf" "git-hly/main"))
