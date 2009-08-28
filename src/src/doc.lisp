(in-package #:js-proc)

;
; a doc-chunk is a pair (schema . text)
;

(defun not-space-p (c)
  (and (char/= #\space c) (char/= #\tab c)))

(defun trim-chunk (text)
  ;;text is a vector of strings
  ;;trim any leading/trailing blank lines
  ;;trim the maximum but same number of spaces from each line in text, ignoring blank lines
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

(defun make-doc-chunk (schema text)
  (cons schema (trim-chunk text)))

(defun doc-chunk-schema (chunk)
  (car chunk))

(defun doc-chunk-text (chunk)
  (cdr chunk))

;
; a doc-section is a vector of zero or more chunks
;
(defun doc-section-push-chunk (section chunk)
  (if section
     (progn (vector-push-extend chunk section) section)
     (make-array 1 :initial-contents (list chunk) :element-type 'cons :fill-pointer 1 :adjustable t)))

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
; a rt (return/throw) is a triple (type . (semantics . condition))
;
(defun make-rt-item (type &optional semantics condition)
  (cons type (cons semantics condition)))

(defun rt-item-type (rt-item)
  (car rt-item))

(defun rt-item-semantics (rt-item)
  (car (cdr rt-item)))

(defun rt-item-condition (rt-item)
  (cdr (cdr rt-item)))

;
; a param (lambda list parameter) is a pair (name . types), types is a vector of pairs (type . semantics).
;
(defun make-param (name &optional type semantics)
  (cons name (make-array 1 :initial-contents (list (cons type semantics)) :element-type 'cons :fill-pointer 1 :adjustable t)))

(defun param-push-ts (param type semantics)
  (vector-push-extend (cons type semantics) (cdr param)))

(defun param-ts-length (param)
  (length (cdr param)))

(defun param-name (param)
  (car param))

(defun param-ts (param i)
  (aref (cdr param) i))

(defun param-type (param i)
  (car (param-ts param i)))

(defun param-semantics (param i)
  (cdr (param-ts param i)))

;
; a doc is holds all the documentation for a single code entity
;
(defstruct doc
  sdoc     ;doc-section--short documentation
  ldoc     ;doc-section--long documentation
  type     ;(:namespace | :type | :const | :enum | :variable | :function | :class)--the type of this documented entity
  requires ;vector of require items--the requirements/prerequisites to use this entity
  returns  ;vector of rt-item--possible return values
  throws   ;vector of rt-item--possible thrown values
  params   ;vector of param--the lambda list for a function
  errors   ;vector of doc-section--possible error/abnormal conditions
  supers   ;vector of string--superclasses
  members  ;hash (name -> doc)--set of member methods/attributes for a class/object
  refs     ;vector of string--references
  location ;quadruple as a list--(start-line start-char end-line end-char) location of the entity in the source resource
  source   ;resource-ctrl--the resource that sourced this entity
)

(defun doc-push-sdoc-chunk (doc chunk)
  (setf (doc-sdoc doc) (doc-section-push-chunk (doc-sdoc doc) chunk)))

(defun doc-push-ldoc-chunk (doc chunk)
  (setf (doc-ldoc doc) (doc-section-push-chunk (doc-ldoc doc) chunk)))

(defun doc-vector-push-item (item vector)
  (if vector
      (progn (vector-push-extend item vector) vector)
      (make-array 1 :initial-contents (list item) :element-type (type-of item) :fill-pointer 1 :adjustable t)))

(defun doc-push-require (doc item)
  (setf (doc-requires doc) (doc-vector-push-item item (doc-requires doc))))

(defun doc-push-return (doc item)
  (setf (doc-returns doc) (doc-vector-push-item item (doc-returns doc))))

(defun doc-push-throws (doc item)
  (setf (doc-throws doc) (doc-vector-push-item item (doc-throws doc))))

(defun doc-push-param (doc item)
  (setf (doc-params doc) (doc-vector-push-item item (doc-params doc))))

(defun doc-push-error (doc item)
  (setf (doc-errors doc) (doc-vector-push-item item (doc-errors doc))))

(defun doc-push-member (doc name member)
  (let ((members (doc-members doc)))
    (if members
        (setf (gethash name members) member)
        (progn 
          (setf (doc-members doc) (make-hash-table))
          (doc-push-member doc name member)))))

(defun doc-push-ref (doc item)
  (setf (doc-refs doc) (doc-vector-push-item item (doc-refs doc))))

(defun doc-chunk-tag (chunk)
  (case (doc-chunk-schema chunk)
    (:mu "markup")
    (:note "note")
    (:warn "warn")
    (t "markdown")))

(defun dump-doc-chunk-to-xml (chunk)
  (xml-emitter:with-tag ((doc-chunk-tag chunk))
    (xml-emitter:xml-out (doc-chunk-text chunk) :indent nil)))

(defun dump-doc-section-to-xml (name section)
  (xml-emitter:with-tag (name)
    (map nil #'dump-doc-chunk-to-xml section)))
 
(defun dump-doc-item-to-xml (name doc)
  (xml-emitter:with-tag ((doc-type doc) (list (list "name" name)))
    (dump-doc-section-to-xml "sdoc" (doc-sdoc doc))
    (dump-doc-section-to-xml "ldoc" (doc-ldoc doc))))

(defun dump-doc-items (stream)
  (with-xml-output (stream) 
    (maphash #'dump-doc-item-to-xml *doc-items*)))

#|
(defstruct doc
  sdoc     ;doc-section--short documentation
  ldoc     ;doc-section--long documentation
  type     ;(:namespace | :type | :const | :enum)--the type of this documented entity
  requires ;vector of pairs of (type(string), value(string))--the requirements/prerequisites to use this entity
  returns  ;vector of triples of (type(string), semantics(doc-section), condition(doc-section))--possible return values
  throws   ;vector of triples of (type(string), semantics(doc-section), condition(doc-section))--possible thrown values
  params   ;vector of parameters--the lambda list for a function
  errors   ;vector of doc-section--possible error/abnormal conditions
  supers   ;vector of string--superclasses
  members  ;hash (name -> doc)--set of member methods/attributes for a class/object
  refs     ;vector of string--references
  location ;quadruple as a list--(start-line start-char end-line end-char) location of the entity in the source resource
  source   ;resource-ctrl--the resource that sourced this entity
)
|#