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
(load "build-asdf-only.lisp")
