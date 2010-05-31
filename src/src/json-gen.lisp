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
           (escape-string (string &optional (quote t))
             (with-output-to-string (stream)
               (and quote (write-char #\" stream))
               (dotimes (i (length string))
                 (let ((c (char string i))) 
                   (case c
                     (#\newline (write-string "\\n" stream))
                     ((#\" #\' #\\) (write-char #\\ stream) (write-char c stream))
                     (t (write-char c stream)))))
               (and quote (write-char #\" stream))))

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
             (if (eq (car chunk) :md)
                 (escape-string (cdr chunk))
                 (concatenate 'string "[" (symbol-name (car chunk)) "," (escape-string (cdr chunk)) "]")))

           
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

           (dump-type-section-vector (types)
             ;;types is a vector of (type . section)
             (cat-array (map 'vector #'dump-type-section types) t))

           (dump-returns-throws (returns type)
             ;;returns is type-section-vector
             (and (plusp (length returns)) 
                  (cat-prop-value (or (and (eq type :returns) "returns") "exceptions") (dump-type-section-vector returns))))

           (dump-param (param)
             ;;param is a cons (name . type-section-vector)
             (cat-pair (quote-string (car param)) (dump-type-section-vector (cdr param))))

           (dump-params (params)
             ;;params is a vector of (name . type-section-vector)
             (and (plusp (length params))
                  (cat-prop-value "params" (cat-array (map 'vector #'dump-param params) t))))

           (dump-overloads (overloads)
             ;;overloads is a list of function doc items
             (if overloads
                 (cat-prop-value "overloads" (cat-array (map 'vector (lambda (item) (dump-doc-item nil item) ) overloads) t))))

           (dump-property (property)
             ;;property is a cons (name . expr), name and expr are asn's
             ;the comment may be on either the name of the expr
             (let* ((name-asn (car property))
                    (value-asn (cdr property))
                    (name (token-value (asn-children name-asn)))
                    (doc (or (asn-doc name-asn) (asn-doc value-asn))));doc could be nil
               (if doc
                   (concatenate 'string (quote-string name) "," (dump-doc-item nil doc))
                   (concatenate 'string (quote-string name) "," "{" (dump-source-location (asn-location value-asn)) "}"))))

           (dump-properties (properties)
             ;properties is a list of (name . expr); name and expr are ast's
             (and (plusp (length properties))
                  (cat-prop-value "props" (cat-array (map 'vector #'dump-property properties) t))))

           (dump-member (class-name member)
             ;;member is a cons (name . expr), name and expr are asn's
             ;;-or- a cons (name . doc), name a string and doc a doc
             ;the comment may be on either the name of the expr
             (if (typep (car member) 'string)
                 (dump-doc-item (concatenate 'string class-name "." (car member)) (cdr member) t)
                 (let* ((name-asn (car member))
                        (value-asn (cdr member))
                        (name (token-value (asn-children name-asn)))
                        (doc (or (asn-doc name-asn) (asn-doc value-asn))));doc could be nil
                   (if doc
                       (progn
                         (setf (doc-location doc) (sum-locations (asn-location name-asn) (asn-location value-asn)))
                         (dump-doc-item (concatenate 'string class-name "." name) doc t))))))

           (dump-types (types)
             ;;types is a type-section-vector
             (and 
              (plusp (length types))
              (cat-prop-value "types" (dump-type-section-vector types))))

           (dump-source-name (source)
             (and source (cat-prop-value "src" (quote-string (resource-name source)))))

           (dump-bd-doc-locs (locs) 
             (if locs
                 (cat-prop-value "docLocs" (cat-array (map 'vector (lambda (loc) (format nil "[~A,~A]" (location-start-line loc) (location-end-line loc))) locs)))))

           (dump-source-location (loc)
             (if loc
                 (format nil "loc: [~A,~A,~A,~A]" (location-start-line loc) (location-start-char loc) (location-end-line loc) (location-end-char loc))))

           (dump-source (text)
             ;text is a vector of strings
             (let ((result (reduce #'(lambda (l1 l2) (concatenate 'string l1 comma-new-line-str l2))
                                   (map 'vector #'(lambda (line) (concatenate 'string "\"" (escape-string line nil) "\"")) text))))
               (cat-prop-value "code" (concatenate 'string "[" result "]"))))

           (dump-type (type) 
             (concatenate 'string "T" (string-downcase type)))
             ;(format nil "s(\"~A_\")" (string-downcase type)))

           (dump-name (name type)
             (case type
               (:resource (concatenate 'string "\"resources." name "\""))
               (:module (concatenate 'string "\"modules." name "\""))
               (otherwise (quote-string name))))

           (dump-flags (flags)
             (if flags
                 (cat-prop-value "flags" (cat-array (map 'vector (lambda (flag) (concatenate 'string "F" (string-downcase flag))) flags)))))

           (dump-imember (class-member)
             (if class-member
                 "imember:1"))

           (dump-supers (supers) 
             (if supers
                 (cat-prop-value "supers" (cat-array (map 'vector (lambda (name) (quote-string name)) supers)))))

           (dump-requires (requires)
             (if requires
                 (cat-prop-value "requires" (cat-array (map 'vector (lambda (name) (quote-string name)) requires)))))
                 
           (dump-require (requires)
             (if requires
                 (cat-prop-value "require" (cat-array (map 'vector (lambda (name) (quote-string name)) requires)))))
               
           (dump-modules (modules)
             (if modules
                 (cat-prop-value "modules" (cat-array (map 'vector (lambda (name) (quote-string name)) modules)))))                 
               
           (dump-defining-module (module)
             (if module
                 (concatenate 'string "module:" (quote-string module))))

           (dump-doc-item (name item &optional (class-member nil))
             ;;these fixups should go into another module
             (if (and (eq (doc-type item) :resource) (not (doc-sdoc item)) (doc-modules item))
                 (setf (doc-sdoc item) (doc-section-push-chunk (make-doc-chunk :md (concatenate 'string "Defines the module " (first (doc-modules item))) nil))))

;               (setf (doc-sdoc item) (concatenate 'string "Defines the module " (first (doc-modules item)))))

             (let ((result (cat-prop-value "type" (dump-type (doc-type item)))))
               (setf result (cat-prop result (dump-sdoc (doc-sdoc item))))
               (setf result (cat-prop result (dump-imember class-member)))
               (setf result (cat-prop result (dump-ldoc (doc-ldoc item))))
               (setf result (cat-prop result (dump-flags (doc-flags item))))
               (setf result (cat-prop result (dump-params (doc-params item))))
               (setf result (cat-prop result (dump-overloads (doc-overloads item))))
               (setf result (cat-prop result (dump-returns-throws (doc-returns item) :returns)))
               (setf result (cat-prop result (dump-returns-throws (doc-throws item) :throws)))
               (setf result (cat-prop result (dump-properties (doc-properties item))))
               (setf result (cat-prop result (dump-source-name (doc-source item))))
               (setf result (cat-prop result (dump-source-location (doc-location item))))
               (setf result (cat-prop result (dump-types (doc-types item))))
               (setf result (cat-prop result (dump-supers (doc-supers item))))
               (setf result (cat-prop result (dump-requires (doc-requires item))))
               (setf result (cat-prop result (dump-require (doc-require item))))
               (setf result (cat-prop result (dump-modules (doc-modules item))))
               (setf result (cat-prop result (dump-defining-module (doc-defining-module item))))
               (if (eq (doc-type item) :resource)
                   (setf 
                    result (cat-prop result (dump-bd-doc-locs (doc-bd-doc-blocks item)))
                    result (cat-prop result (dump-source (resource-text (doc-source item))))))
               (setf result (concatenate 'string lbrace-new-line-str result rbrace-new-line-str))
               (if name
                 (setf result (concatenate 'string (dump-name name (doc-type item)) ":" result)))
               (map nil (lambda (member)
                          (let ((doc-string (dump-member name member)))
                            (if doc-string
                                (setf result (concatenate 'string result "," doc-string))))) (doc-members item))
               (if (doc-kwargs item)
                   (setf result (concatenate 'string result "," (dump-doc-item (concatenate 'string name ".kwargs") (doc-kwargs item)))))
               result))
           )
    
    (let ((result ""))
      (maphash #'(lambda (name item)
                   (setf result (concatenate 'string result "," (dump-doc-item (car name) item))))
               doc-items)
      (format stream "~A~%" (subseq result 1)))))
