(in-package #:js-proc)

;;
;; Convert the source into a vector of strings, one item per line
;;

(defun read-source (stream)
  (let ((text (make-array 100 :element-type 'string :fill-pointer 0 :adjustable t)))
      (do ((line (read-line stream nil 'eof)
                 (read-line stream nil 'eof)))
           ((eql line 'eof))
           (vector-push-extend line text))
    text))
      
(defun read-source-filename (filename)
  (with-open-file (in filename)
    (read-source in)))

(defun read-source-string (string)
  (with-input-from-string (in string)
    (read-source in)))
