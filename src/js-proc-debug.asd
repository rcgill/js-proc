(declaim (optimize (speed 0)(space 0)(debug 3)))

(asdf:defsystem #:js-proc
  :depends-on (#:cl-ppcre)
  :components
  ((:module :src
            :components (
                         (:file "package")
                         (:file "reader":depends-on ("package"))
                         (:file "util" :depends-on ("package"))
                         (:file "tokenize" :depends-on ("util"))
                         (:file "parse" :depends-on ("tokenize"))
                         (:file "drivers" :depends-on ("reader" "tokenize" "parse"))
                         (:file "doc-gen" :depends-on ("parse"))))))

(asdf:oos 'asdf:load-op :js-proc)

