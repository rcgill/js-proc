(declaim (optimize (speed 0)(space 0)(debug 3)))

(asdf:defsystem #:js-proc
  :depends-on (#:cl-ppcre #:xml-emitter)
  :components
  ((:module :src
            :components (
                         (:file "package")
                         (:file "reader":depends-on ("package"))
                         (:file "util" :depends-on ("package"))
                         (:file "tokenize" :depends-on ("util"))
                         (:file "parse" :depends-on ("tokenize"))
                         (:file "drivers" :depends-on ("reader" "tokenize" "parse"))
                         (:file "doc" :depends-on ("package"))
                         (:file "doc-gen" :depends-on ("parse" "doc"))
                         (:file "xml-gen" :depends-on ("doc"))
                         (:file "json-gen" :depends-on ("doc"))))))

(asdf:oos 'asdf:load-op :js-proc)

 