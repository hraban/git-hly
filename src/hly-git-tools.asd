(asdf:defsystem :hly-git-tools
     :description "Various git utilities by Hraban"
     :version "0.1"
     :author "Hraban Luyat"
     :build-operation "program-op"
     :build-pathname "../dist/hly-git-tools"
     :entry-point "hly-git-tools:main"
     :depends-on (:alexandria
                  :arrow-macros
                  :inferior-shell
                  :uiop)
     :components ((:file "package")
                  (:file "os")
                  (:file "cmd")
                  (:file "cmd-git-graft")
                  (:file "app")))
