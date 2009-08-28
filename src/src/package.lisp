(cl:defpackage #:js-proc
  (:use #:cl #:xml-emitter)
  (:import-from :xml-emitter :with-xml-output)
  (:export #:token-type #:token-value #:token-line #:token-char #:token-newline-before
           #:lex-js #:parse-js #:parse-js-string
           #:js-parse-error #:js-parse-error-line #:js-parse-error-character))
