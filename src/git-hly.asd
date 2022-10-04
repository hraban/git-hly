(asdf:defsystem :git-hly
     :description "Various git utilities by Hraban"
     :version "0.1"
     :author "Hraban Luyat"
     :build-operation "program-op"
     :build-pathname "../dist/git-hly"
     :entry-point "git-hly:main"
     :depends-on (:alexandria
                  :arrow-macros
                  :inferior-shell
                  :trivia
                  :trivia.ppcre
                  :uiop)
     :components ((:file "package")
                  (:file "os")
                  (:file "cmd")
                  (:file "cmd-graft")
                  (:file "cmd-pullfetch")
                  (:file "app")))
