(in-package #:js-proc)

;
; a doc-chunk is a pair (pragma . text)
;

(defun not-space-p (c)
  (and (char/= #\space c) (char/= #\tab c)))

(defun trim-chunk (text)
  ;;text is a vector of strings
  ;;trim any leading/trailing blank lines
  ;;left trim the maximum but same number of spaces from each line in text, ignoring blank lines
  ;;join the result with \n and return
  (let* ((line-count (length text))
         (start (do ((i 0 (incf i))
                     (end line-count))
                    ((or (= i end) (aref text i))
                     i)))
         (end (do ((i (1- line-count) (decf i)))
                  ((or (<= i start) (aref text i))
                   i))))
    ;start/end or the first/last non-blank lines   
    (if (= start line-count)
        ""
        (let ((min-spaces (do ((min-spaces 10000) ;10K is arbitrary (assuming no line is more than 10K chars!)
                               (i start (incf i)))
                              ((or (> i end) (zerop min-spaces))
                               min-spaces)
                            (let* ((s (aref text i))
                                   (s-length (length s)))
                              ;ignore blank lines
                              (if (> s-length 0)
                                  (setf min-spaces (min min-spaces (position-if #'not-space-p s))))))))
          (do* ((i (1+ start) (incf i))
               (acc (subseq (aref text start) min-spaces)))
              ((> i end)
               acc)
            (let ((s (aref text i)))
              (setf acc (concatenate 'string acc #(#\newline) 
                                     (if s
                                         (subseq s min-spaces)
                                         "")))))))))

(defun make-doc-chunk (pragma text &optional (trim t))
  (cons pragma (or (and trim (trim-chunk text)) text)))

(defun doc-chunk-pragma (chunk)
  (car chunk))

(defun doc-chunk-text (chunk)
  (cdr chunk))

(defun (setf doc-chunk-text) (text chunk)
  (setf (cdr chunk) text))

;
; a doc-section is a vector of zero or more chunks
;
(defun make-doc-section ()
  (make-array 1 :element-type 'cons :fill-pointer 0 :adjustable t))

(defun doc-section-push-chunk (chunk &optional section)
  (let ((section (or section (make-doc-section))))
    (vector-push-extend chunk section)
    section))
;
; a require is a (type . value)
;
(defun make-require-item (type value)
  (cons type value))

(defun require-item-type (require-item)
  (car require-item))

(defun require-item-value (require-item)
  (cdr require-item))

;
; a rt (return/throw) is a triple (type . (result . condition)); result and condition are doc-sections
; doc returns / doc throws are vectors of rt
(defun make-doc-returns()
  (make-array 1 :element-type 'cons :fill-pointer 0 :adjustable t))

;
; a param (function parameter) is a pair (name . types), types is a vector of pairs (type . semantics), semenatics is a doc-section
; doc params is vector of param
;
(defun make-doc-params ()
  (make-array 1 :element-type 'cons :fill-pointer 0 :adjustable t))

(defun make-doc-param (name)
  (cons name (make-array 1 :element-type 'cons :fill-pointer 0 :adjustable t)))

(defun doc-param-push-type (param type section)
  (vector-push-extend (cons type section) (cdr param)))

(defun doc-types-push-type (doc type section)
  (if (doc-types doc)
      (vector-push-extend (cons type section) (doc-types doc))
      (progn
        (setf (doc-types doc) (make-array 1 :element-type 'cons :fill-pointer 0 :adjustable t))
        (doc-types-push-type doc type section))))

(defun doc-push-return (doc type section)
  (if (doc-returns doc)
      (vector-push-extend (cons type section) (doc-returns doc))
      (progn
        (setf (doc-returns doc) (make-array 1 :element-type 'cons :fill-pointer 0 :adjustable t))
        (doc-push-return doc type section))))

;;TODO--this is sloppy; these two functions above/below should be refactored.
(defun doc-push-throw (doc type section)
  (if (doc-throws doc)
      (vector-push-extend (cons type section) (doc-throws doc))
      (progn
        (setf (doc-throws doc) (make-array 1 :element-type 'cons :fill-pointer 0 :adjustable t))
        (doc-push-throw doc type section))))
      
;
; a doc is holds all the documentation for a single code entity
;
(defstruct doc
  type       ;(:namespace | :type | :const | :enum | :variable | :function | :class | :resource)--the type of this documented entity
  flags      ;list of keywords
  sdoc       ;doc-section--short documentation
  ldoc       ;doc-section--long documentation
  requires   ;list of items a resource includes (e.g., like dojo.require of C/C++ #include)
  require    ;list of items the user should include to gain access to the resource's contents
  provides   ;vector of provided items
  returns    ;vector of vector of (type . doc-section)
  throws     ;vector of rt-item--possible thrown values
  params     ;vector of param--the lambda list for a function
  types      ;vector of types--vector of (type . section); used to document property types for an object
  errors     ;vector of doc-section--possible error/abnormal conditions
  supers     ;vector of string--superclasses
  members    ;hash (name -> ast)--set of member methods/attributes for a class
  properties ;list (name . expr)--the properties of an object; name and expr are asn's
  refs       ;vector of string--references
  inotes     ;doc-section--implementation notes
  location   ;quadruple as a list--(start-line start-char end-line end-char) location of the entity in the source resource
  source     ;resource--the resource that sourced this entity
)

(defun doc-get-sdoc (doc)
  (or (doc-sdoc doc) (setf (doc-sdoc doc) (make-doc-section))))

(defun doc-get-ldoc (doc)

  (or (doc-ldoc doc) (setf (doc-ldoc doc) (make-doc-section))))

(defun doc-get-inotes (doc)
  (or (doc-inotes doc) (setf (doc-inotes doc) (make-doc-section))))

(defun doc-get-returns (doc)
  (or (doc-returns doc) (setf (doc-returns doc) (make-doc-returns))))

(defun doc-get-throws (doc)
  (or (doc-returns doc) (setf (doc-returns doc) (make-doc-returns))))

(defun doc-vector-push-item (item vector)
  (if vector
      (progn (vector-push-extend item vector) vector)
      (make-array 1 :initial-contents (list item) :element-type (type-of item) :fill-pointer 1 :adjustable t)))

(defun doc-push-param (doc item)
  (setf (doc-params doc) (doc-vector-push-item item (doc-params doc))))

(defun doc-push-error (doc item)
  (setf (doc-errors doc) (doc-vector-push-item item (doc-errors doc))))

(defun doc-push-ref (doc item)
  (setf (doc-refs doc) (doc-vector-push-item item (doc-refs doc))))

(defun doc-chunk-tag (chunk)
  (case (doc-chunk-pragma chunk)
    (:mu "markup")
    (:note "note")
    (:warn "warn")
    (:code "code")
    (:todo "todo")
    (:todoc "todoc")
    (:inote "inote")
    (t "markdown")))

(defun doc-push-require (doc text)
  (dolist (s (cl-ppcre:split "\\s+" (string-trim " " text)))
    (push s (doc-require doc))))