;; Raw ASDF-only part of the build process. Call this if you know for sure ASDF
;; can find all dependencies.

(require :asdf)

(push (merge-pathnames #p"./") asdf:*central-registry*)

(sb-ext:disable-debugger)

;; An incestuous twist: also load the command, to get all binary names, to
;; automatically create symlinks for every command. This really is a massive
;; hack, also registering this as a pre-image-dump hook, because ideally I’d
;; want to run this /after/ building but that’s just too much work. Because asdf
;; program-op kills SBCL. Whatever. 🤷‍♀️ I don’t mind this too much because in
;; the end, I need all these systems loaded in the main program, too, so it
;; doesn’t add any noise to the binary.
(asdf:load-system "git-hly")
(asdf:load-system "inferior-shell")
(asdf:load-system "uiop")
(uiop:register-image-dump-hook
 (lambda ()
   (dolist (cmd (git-hly::cmd-names))
     (uiop:with-current-directory ("./bin/")
       (inferior-shell:run `(ln -sf "git-hly" ,(format NIL "~(git-~A~)" cmd)))))))

;; Build the binary
(asdf:make "git-hly")

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
