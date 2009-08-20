(in-package #:js-proc)

#|
  Multi-line, multi-chunk pragmas:

  //mu(docbook)  --markup follows; () gives schema
  //mu(md)       --markdown follows
  //mu(markdown) --markdown follows
  ///            --end any previous pragma; return to markdown

  Single-line pragmas:

  //#          --pragma for eval preprocessing
  //namespace  --object that serves as a namespace
  //const      --constant variable
  //enum       --object than serves as an enumeration
  //type       --the documented item is a type

  Multi-line, multi-chunk, or single-line pragmas:

  //n      --note section
  //note   --saa
  //warn   --warn section
  //w      --saa
  //return --a return section
  //r      --returns (either a return section or a single item in a return section)
  //throws --throws
  //t      --throws (either a throws sectino of a single item in a throw section)
  //c      --condition in a return/throw section or code otherwise
  //code   --code
  //todo   --to do note
  //todoc  --to document note
  //in     --implementation note
  //inote  --saa
|#


;;
;; The document entity machinery serves to manipulate all of the documentation
;; associated with a single document entity which is encapsulated in a "doc" structure.
;;
(defun make-doc-chunk (schema text)
  (cons schema text))

(defun doc-chunk-schema (chunk)
  (car chunk))

(defun doc-chunk-text (chunk)
  (cdr chunk))

(defun section-push-chunk (section chunk)
  (cond
    ((eq section nil) 
     chunk)
    ((consp section) 
     (make-array 2 :initial-contents (list section chunk) :element-type 'cons :fill-pointer 2 :adjustable t))
    (t 
     (progn (vector-push-extend chunk section) section))))

(defun section-length (section)
  (cond
    ((eq section nil) 0)
    ((consp section) 1) 
    (t (length section))))

(defun section-get-chunk (section i)
  (if (consp section)
      section
      (aref section i)))

(defun make-require (type value)
  (cons type value))

(defun require-type (require-item)
  (car require-item))

(defun require-value (require-item)
  (cdr require-item))

(defun make-rt-item (type &optional semantics condition)
  (cons type (cons semantics condition)))

(defun rt-item-type (rt-item)
  (car rt-item))

(defun rt-item-semantics (rt-item)
  (car (cdr rt-item)))

(defun rt-item-condition (rt-item)
  (cdr (cdr rt-item)))

(defun make-param (name &optional type semantics)
  (cons name (make-array 1 :initial-contents (list (cons type semantics)) :element-type 'cons :fill-pointer 1 :adjustable t)))

(defun param-name (param)
  (car param))

(defun param-push-ts (param type semantics)
  (vector-push-extend (cons type semantics) (cdr param)))

(defun param-ts-length (param)
  (length (cdr param)))

(defun param-ts (param i)
  (aref (cdr param) i))

(defun param-type (param i)
  (car (param-ts param i)))

(defun param-semantics (param i)
  (cdr (param-ts param i)))

(defstruct doc
  sdoc     ;doc-section--short documentation
  ldoc     ;doc-section--long documentation
  type     ;string--the type of this documented entity
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

(defun doc-push-sdoc-chunk (doc chunk)
  (setf (doc-sdoc doc) (section-push-chunk (doc-sdoc doc) chunk)))

(defun doc-push-ldoc-chunk (doc chunk)
  (setf (doc-ldoc doc) (section-push-chunk (doc-ldoc doc) chunk)))

(defun push-item (item vector)
  (if vector
      (progn (vector-push-extend item vector) vector)
      (make-array 1 :initial-contents (list item) :element-type (type-of item) :fill-pointer 1 :adjustable t)))

(defun doc-push-require (doc item)
  (setf (doc-requires doc) (push-item item (doc-requires doc))))

(defun doc-push-return (doc item)
  (setf (doc-returns doc) (push-item item (doc-returns doc))))

(defun doc-push-throws (doc item)
  (setf (doc-throws doc) (push-item item (doc-throws doc))))

(defun doc-push-param (doc item)
  (setf (doc-params doc) (push-item item (doc-params doc))))

(defun doc-push-error (doc item)
  (setf (doc-errors doc) (push-item item (doc-errors doc))))

(defun doc-push-member (doc name member)
  (let ((members (doc-members doc)))
    (if members
        (setf (gethash name members) member)
        (progn 
          (setf (doc-members doc) (make-hash-table))
          (doc-push-member doc name member)))))

(defun doc-push-ref (doc item)
  (setf (doc-refs doc) (push-item item (doc-refs doc))))


(defun test-doc ()
  (let ((d (make-doc)))
    (format t "1: ~A~%" d)

    (doc-push-sdoc-chunk d (make-doc-chunk "1- md" "1- This is some markdown"))
    (format t "1: ~A~%" d)

    (doc-push-sdoc-chunk d (make-doc-chunk "2- md" "2- This is some markdown"))
    (format t "1: ~A~%" d)

    (doc-push-sdoc-chunk d (make-doc-chunk "3- md" "3- This is some markdown"))
    (format t "1: ~A~%" d)

    (doc-push-ldoc-chunk d (make-doc-chunk "4- md" "4- This is some markdown"))
    (format t "1: ~A~%" d)

    (doc-push-ldoc-chunk d (make-doc-chunk "5- md" "5- This is some markdown"))
    (format t "1: ~A~%" d)

    (doc-push-ldoc-chunk d (make-doc-chunk "6- md" "6- This is some markdown"))
    (format t "1: ~A~%" d)

    (doc-push-require d (make-require "7- dojo-require" "7- bd"))
    (format t "1: ~A~%" d)

    (doc-push-require d (make-require "8- cpp-include" "8- vector"))
    (format t "1: ~A~%" d)

    (doc-push-require d (make-require "9- c-include" "9- stdio.h"))
    (format t "1: ~A~%" d)

    (doc-push-return d (make-rt-item "10- int" "10- some return semantic" "10- some return condition"))
    (format t "1: ~A~%" d)

    (doc-push-return d (make-rt-item "11- int" "11- some return semantic" "11- some return condition"))
    (format t "1: ~A~%" d)

    (doc-push-return d (make-rt-item "12- int" "12- some return semantic" "12- some return condition"))
    (format t "1: ~A~%" d)

    (doc-push-return d (make-rt-item "13- int" "13- some return semantic"))
    (format t "1: ~A~%" d)

    (doc-push-return d (make-rt-item "14- int"))
    (format t "1: ~A~%" d)

    (doc-push-return d (make-rt-item "15- int" (make-doc-chunk "15-md" "15- some return semantic") (make-doc-chunk "15-md" "15- some return condition")))
    (format t "1: ~A~%" d)

    (doc-push-param d (make-param "a" "int" "some parameter named 'a' that's an int"))


))

(defun compile-text (text)
  (map 'list (lambda (s)
               (multiple-value-bind (match match-strings) (cl-ppcre:scan-to-strings "//(\\w+)(\\s+\\S.*)?" s)
                 (if match
                     (let ((pragma (aref match-strings 0))
                           (other (aref match-strings 1)))
                       (if other
                           (cons pragma (concatenate 'string (make-string (length pragma) :initial-element #\space) other))
                           (cons pragma nil)))
                     (cons nil (subseq s 2)))))
       text))


(defun compile-raw-doc (text)
  (let ((doc (make-doc)))
    (setf text (compile-text text))
    (format t "~A~%" text)
    doc))

(defun create-*-doc (
  ast ;the ast to document
  raw-doc ;the raw documentation associated that was attached to a node higher in the tree
)
  (setf raw-doc (or raw-doc (get-ast-comment ast)))
  (when raw-doc
    (let ((doc (compile-raw-doc raw-doc)))
      (case (get-ast-type ast)
        (:function 
         (create-function-doc ast doc))
        (:object 
         (create-object-doc ast doc))
        (:array 
         (create-array-doc ast doc))
        (t 
         (create-variable-doc ast doc))))))

(defun create-variable-doc (
  ast ;an object literal production
  doc ;the comment that was attached to the assignment operator (= or :)
)
  doc)

(defun create-array-doc (
  ast ;an object literal production
  doc ;the comment that was attached to the assignment operator (= or :)
)
  doc)

(defun create-object-doc (
  ast ;an object literal production
  doc ;the comment that was attached to the assignment operator (= or :)
)
  (setf (doc-members doc) 
        (delete-if (lambda (item) (cdr item)) 
                   (mapcar (lambda (member)
                             (let ((name (first member))
                                   (raw-doc (second member))
                                   (rhs (third member)))
                               (cons name (create-*-doc rhs raw-doc))))
                           (second ast))))
  doc)

(defun create-function-doc (
  ast ;an object literal production
  doc ;the comment that was attached to the assignment operator (= or :)
)
  ;params is a list in third of ast
  doc)

;;
;; These are the special processing functions
;;
#|
(defun |dojo.declare| (ast)
  (labels ((get-super-list (supers)
             (if (eq (get-ast-type supers) :array)
                 (mapcar (lambda (ast) (get-ast-name ast)) (second supers))
                 nil)))
    (let ((args (third ast)))
      (if (/= (length args) 3)
          (format t "expected three arguments to dojo.declare~%")
          (let ((members (create-object-doc (third args) "" nil)))
            (if members
                (append-doc (cdr (first args))
                            (make-class-doc 
                             :source *current-source* 
                             :location nil 
                             :doc (get-ast-comment (third args))
                             :supers (get-super-list (second args))
                             :members (object-doc-members members)))
                (format t "the dojo.declare'd class ~A is undocumented ~%" (cdr (first args)))))))))

(defun |dojo.mixin| (ast)
  (when (eq (length (third ast)) 2)
    (let* ((name (get-ast-name (first (third ast))))
           (arg2 (second (third ast)))
           (members (and (eq (get-ast-type arg2) :object) (create-object-doc arg2 "force-doc" nil))))
      (when (and name (or (< (length name) 4) (not (equal (subseq name 0 4) "this"))))
        (mapc (lambda (property)
                (when (nonempty-doc (cdr property))
                  (append-doc (concatenate 'string name "." (car property))
                              (cdr property))))
              (object-doc-members members))))))
|#


;;
;; This is the document stack
;;
(defparameter *doc-items*
  (make-hash-table :test 'equal))

(defun append-doc (key item)
  (if (gethash key *doc-items*)
      (format t "ERROR: multiple doc items for ~A~%." key)
      (setf (gethash key *doc-items*) item)))

;;
;; These functions decode an ast node
;;
(defun get-ast-type (ast)
  (node-info-type (car ast)))

(defun get-ast-comment (ast)
  (node-info-comment (car ast)))

(defun get-ast-location (ast)
  (node-info-location (car ast)))

(defun get-ast-name (ast)
  (let ((type (get-ast-type ast)))
    (cond
      ((eql type :name) (second ast))
      ((eql type :dot) (concatenate 'string (get-ast-name (second ast)) "." (third ast)))
      (t nil))))

(defun get-ast-assign-attrib (ast attrib)
  (case attrib
    (:type (second ast))
    (:left (third ast))
    (:right (fourth ast))))

;;
;; These are the ast node processing functions
;;
(defun proc-assign (ast)
  (let* ((name (get-ast-name (get-ast-assign-attrib ast :left)))
         (raw-doc (get-ast-comment ast))
         (rhs (get-ast-assign-attrib ast :right))
         (doc (create-*-doc rhs raw-doc)))
    (when (and name doc)
      (setf (doc-location doc) (get-ast-location ast))
      (append-doc name doc))))

(defun proc-call (ast)
  (let* ((name (get-ast-name (second ast)))
         (f (and name (find-symbol name))))
    (when f (funcall f ast)))
  (traverse (second ast)))

(defparameter *processors*
  (let ((procs (make-hash-table)))
    (dolist (node-type '(:assign :call))
      (setf (gethash node-type procs) (find-symbol (concatenate 'string "PROC-" (symbol-name node-type)))))
    procs))

(defun default-processor (ast)
  (dolist (node (cdr ast) nil)
    (traverse node)))

(defun lookup-processor (node-type)
  (or (gethash node-type *processors*) #'default-processor))

(defun traverse (ast)
  (if (and ast (listp ast))
      (if (node-info-p (car ast))
          (funcall (lookup-processor (get-ast-type ast)) ast)
          (progn (traverse (car ast)) (traverse (cdr ast)))))) ;ast may not be a proper list

(defun generate-docs (ast)
  ;;keep the source out of the object for debugging...
  (setf *current-source* nil)

  (clrhash *doc-items*)
  (traverse ast)
  (dump-doc-items)
)

;;for quick testing
(defun dump-doc-items () 
  (maphash (lambda (key value) (format t "~A~%~A~%~%" key value)) 
           *doc-items*))


