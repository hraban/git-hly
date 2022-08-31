#!/usr/bin/env -S sbcl --noinform --script

;; Build script which should work in both Docker and on a host computer (iow a
;; developer's mac laptop).
;;
;; This assumes default quicklisp installation.
;;
;; Based on the instructions in
;; https://lispcookbook.github.io/cl-cookbook/scripting.html

;; For the dependencies
(load (merge-pathnames #p"quicklisp/setup.lisp" (user-homedir-pathname)))

;; For this project. Note xach's reply at
;; <https://old.reddit.com/r/Common_Lisp/comments/lx6al4/loading_an_asdf_system_from_current_directory/>:
;; yes this var is deprecated, but "That doesn’t matter much - it’s easy to
;; re-add if it ever is removed."
(push (merge-pathnames #p"src/") asdf:*central-registry*)

(sb-ext:disable-debugger)

(load "src/hly-git-tools.asd")

;; An incestuous twist: also load the command, to get all binary names, to
;; automatically create symlinks for every command. This really is a massive
;; hack, also registering this as a pre-image-dump hook, because ideally I’d
;; want to run this /after/ building but that’s just too much work. Because asdf
;; program-op kills SBCL. Whatever. 🤷‍♀️ I don’t mind this too much because in
;; the end, I need all these systems loaded in the main program, too, so it
;; doesn’t add any noise to the binary.
(asdf:load-system "hly-git-tools")
(asdf:load-system "inferior-shell")
(asdf:load-system "uiop")
(uiop:register-image-dump-hook
 (lambda ()
   (dolist (cmd (hly-git-tools::cmd-names))
     (uiop:with-current-directory ("./dist/")
       (inferior-shell:run `(ln -sf "hly-git-tools" ,(string-downcase cmd)))))))

;; Build the binary
(asdf:make "hly-git-tools")

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
