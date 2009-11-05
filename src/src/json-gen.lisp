(in-package #:js-proc)

(defparameter comma-new-line-str ",
")

(defparameter colon-new-line-str ":
")

(defparameter new-line-str "
")

(defparameter lbrace-new-line-str "{
")

(defparameter rbrace-new-line-str "}
")

(defun dump-doc-items-to-json (stream doc-items)
  (labels (
           (escape-string (string)
             (with-output-to-string (stream)
               (write-char #\" stream)
               (dotimes (i (length string))
                 (let ((c (char string i))) 
                   (case c
                     (#\newline (write-string "\\n" stream))
                     ((#\" #\' #\\) (write-char #\\ stream) (write-char c stream))
                     (t (write-char c stream)))))
               (write-char #\" stream)))

           (quote-string (string)
             (concatenate 'string "\"" string "\""))

           (cat-prop-value (prop value)
             (concatenate 'string prop colon-new-line-str value))

           (cat-prop (base src)
             (if src
                 (concatenate 'string base comma-new-line-str src)
                 base))

           (cat-array (elements &optional (new-line nil))
             (let ((length (length elements)))
               (cond
                 ((= length 0) 
                  "[]")

                 ((= length 1) 
                  (concatenate 'string "[" (aref elements 0) "]"))

                 (t 
                  (let ((separator (or (and new-line comma-new-line-str) ","))
                        (result (aref elements 0)))
                    (do ((i 1 (1+ i)))
                        ((eq i length))
                      (setf result (concatenate 'string result separator (aref elements i))))
                    (concatenate 'string "[" result "]"))))))

           (cat-pair (first second &optional (new-line nil))
             (concatenate 'string "[" first (or (and new-line comma-new-line-str) ",") second "]"))

           (dump-section-chunk (chunk)
             ;;section chunk is a cons (pragma . text)
             (concatenate 'string "[" (symbol-name (car chunk)) "," (escape-string (cdr chunk)) "]"))
           
           (dump-section (section)
             ;;section is a vector of chunks
             (cat-array (map 'vector #'dump-section-chunk section) t))

           (dump-sdoc (sdoc)
             (and sdoc (cat-prop-value "sdoc" (dump-section sdoc))))

           (dump-ldoc (ldoc)
             (and ldoc (cat-prop-value "ldoc" (dump-section ldoc))))

           (dump-type-section (type-section)
                                        ;type-section is a cons (type . section)
             (cat-pair (escape-string (car type-section)) (dump-section (cdr type-section)) t))

           (dump-type-section-vector (type-section-vector)
             (cat-array (map 'vector #'dump-type-section type-section-vector) t))

           (dump-returns (returns)
             ;;returns is type-section-vector
             (and (plusp (length returns)) 
                  (cat-prop-value "returns" (dump-type-section-vector returns))))

           (dump-param (param)
             ;;param is a cons (name . type-section-vector)
             (cat-pair (quote-string (car param)) (dump-type-section-vector (cdr param))))

           (dump-params (params)
             ;;params is a vector of (name . type-section-vector)
             (and (plusp (length params))
                  (cat-prop-value "params" (cat-array (map 'vector #'dump-param params) t))))

           (dump-property (property)
             ;;property is a cons (name . expr), name and expr are asn's
             (let* ((name-asn (car property))
                    (value-asn (cdr property))
                    (name (token-value (asn-children name-asn)))
                    (doc (or (asn-doc name-asn) (asn-doc value-asn))))
               (concatenate 'string (quote-string name) "," (dump-doc-item nil doc))))

           (dump-properties (properties)
             ;;properties is a list of (name . expr); name and expr are ast's
             (and (plusp (length properties))
                  (cat-prop-value "props" (cat-array (map 'vector #'dump-property properties) t))))

           (dump-types (types)
             ;;types is a type-section-vector
             (and 
              (plusp (length types))
              (cat-prop-value "types" (dump-type-section-vector types))))

           (dump-source (source)
             (and source (cat-prop-value "src" (quote-string (resource-name source)))))

           (dump_type (type) 
             (string-upcase type))

           (dump-doc-item (name item)
             (let ((result (cat-prop-value "type" (dump_type (doc-type item)))))
               (setf result (cat-prop result (dump-sdoc (doc-sdoc item))))
               (setf result (cat-prop result (dump-ldoc (doc-ldoc item))))
               (setf result (cat-prop result (dump-params (doc-params item))))
               (setf result (cat-prop result (dump-returns (doc-returns item))))
               (setf result (cat-prop result (dump-properties (doc-properties item))))
               (setf result (cat-prop result (dump-source (doc-source item))))
               (setf result (cat-prop result (dump-types (doc-types item))))
               (setf result (concatenate 'string lbrace-new-line-str result rbrace-new-line-str))
               (if name
                 (concatenate 'string (quote-string name) ":" result)
                 result)))
           )
    
    (let ((result ""))
      (maphash #'(lambda (name item)
                   (setf result (concatenate 'string result "," (dump-doc-item (car name) item))))
               doc-items)
      (format stream "~A~%" (subseq result 1)))))



