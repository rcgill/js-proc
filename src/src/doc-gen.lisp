(in-package #:js-proc)

(defun make-svector (&optional (first-value nil))
  (let ((result (make-array 100 :element-type 'string :fill-pointer 0 :adjustable t)))
    (if first-value
        (vector-push-extend first-value result))
    result))

;
; take a list of text and sift it into a list sifted lines
;
(defstruct sifted-line
  pragma ;the pragma on the line; nil if none
  args   ;the pragma arguments on the line; nil if none
  text   ;anything and everything after the pragma and args
)

(defparameter *pragmas*
  (let ((pragmas (make-hash-table :test 'equal)))
    (dolist (word '(:mu :namespace :const :enum :type :variable :class :function :mfunction :mvariable :mconst :menum :note :warn :return :throws :code :todo :todoc :inote))
      (setf (gethash (string-downcase (string word)) pragmas) word))
    ;synonms...
    (setf (gethash "n" pragmas) :note)
    (setf (gethash "w" pragmas) :warn)
    (setf (gethash "r" pragmas) :return)
    (setf (gethash "t" pragmas) :throw)
    (setf (gethash "c" pragmas) :code)
    (setf (gethash "in" pragmas) :inote)

    (setf (gethash "/" pragmas) :escape)
    (setf (gethash nil pragmas) :ptype) ;a parameter type specifier
    pragmas))

(defparameter pragma-scanner 
  ; ^((//)(\\s*`)?|(\\s*`))  start of line followed by "//" or "//<spaces>`" or "<spaces>`"
  ; ([^\\s\\(]+)? -- the pragma; one or more of anything other than a space or left parentheses
  ; (\\([^\\)]*\\))? -- the pragma arguments; anything other than a right parentheses enclosed in parentheses.
  ; (\\s+(.*))? everything after the pragma; must have at least one space after the pragma
  ;
  ; notice that it is possible to match with both nil pragma and pragma arguments (e.g., "// the rest")
  ; therefore, a match is known when match is true and one or both of the pragma/pragma arguments is non-nil
  (cl-ppcre:create-scanner "^((//)(\\s*`)?|(\\s*`))([^\\s\\(]+)?(\\([^\\)]*\\))?(\\s+(.*))?"))
  ;                          01   2        3       4            5               6    7

(defparameter //-comment-scanner 
  (cl-ppcre:create-scanner "^//"))

(defparameter non-space-scanner 
  (cl-ppcre:create-scanner "\\S"))

(defun sift-pragmas (text)
  ; map text to a list of sifted-lines
  ; nil pragma implies there was no pragma as given by *pragmas*
  ; text is modified by replacing the pragma positions with spaces, right-trimming, and deleting any leading "//"
  ; if text contain only spaces (after modification, then text is set to nil
  (map 'list 
       (lambda (s)
         (multiple-value-bind (match match-strings) (cl-ppcre:scan-to-strings pragma-scanner s)
           (let* ((pragma (and match 
                              (or (aref match-strings 4) (aref match-strings 5)) ;at least pragma or pragma args must exist to specify a pragma
                              (gethash (aref match-strings 4) *pragmas*) ;and the specified pragma must be a real pragma
                              ))
                  (rest-of-line (and pragma (aref match-strings 7) (string-right-trim '(#\Space #\Tab) (aref match-strings 6))))
                  (spaces (and rest-of-line (make-string (do ;the sum of the lengths of match-strings 2, 3, 4, 5
                                                          ((sum 0)
                                                           (i 2 (incf i)))
                                                          ((> i 5)
                                                           sum)
                                                           (setf sum (+ sum (length (aref match-strings i))))) :initial-element #\space))))
             (if pragma
                 (make-sifted-line :pragma pragma :args (aref match-strings 5) :text (and rest-of-line (concatenate 'string spaces rest-of-line)))
                 (let ((s (cl-ppcre:regex-replace //-comment-scanner s "")))
                     (make-sifted-line :text (and (cl-ppcre:scan non-space-scanner s) (string-right-trim '(#\Space #\Tab) s))))))))
             text))
 
(defun line-pragma (line)
  (sifted-line-pragma (car line)))

(defun line-pragma-args (line)
  (sifted-line-args (car line)))

(defun line-text (line)
  (sifted-line-text (car line)))

(defun blank-line (line)
  (not (line-text line)))


(defun get-simple-chunk (text doc)
  ;always append to ldoc of doc
  (let ((end ;if the next pragma line is an escape, then that line; othewise the first blank line
         (do ((first-blank-line nil)
              (p (cdr text) (cdr p)))
             ((or (not p) (line-pragma p)) 
              (if (and p (eq (line-pragma p) :escape))
                  p
                  first-blank-line))
           (if (and (not first-blank-line) (blank-line p))
               (setf first-blank-line p))))
        (schema (line-pragma text))
        (contents (make-svector)))
    (vector-push-extend (line-text text) contents)
    (do ((p (cdr text) (cdr p)))
        ((eq p end) 
         (doc-push-ldoc-chunk doc (make-doc-chunk schema contents))
         p)
      (vector-push-extend (line-text p) contents))))

(defun get-return-or-throw-block (text doc)
  text)

(defun get-inote-block (text doc)
  text)

(defparameter sdoc-scanner 
  ;notice this is non-greedy for the chunk before the //
  (cl-ppcre:create-scanner "^(.*?)//(.*)$"))
  ;                          0      1

(defun extract-sdoc (schema contents doc)
  ;The short doc is either 
  ;  1. the first non-specific chunk, or...
  ;  2. an escaped part of the first non-specific chunk.  
  ;
  ;In case [1] the chunk is moved completely out of the ldoc and into the sdoc.
  ;In case [2] the part is copied to the sdoc, but remains in the ldoc.
  (do ((i 0 (incf i))
       (end (length contents)))
      ((= i end)
       (doc-push-sdoc-chunk doc (make-doc-chunk schema contents))
       (make-svector))
    (multiple-value-bind (match match-strings) (cl-ppcre:scan-to-strings sdoc-scanner (aref contents i))
      (if match
          (let ((sdoc-contents (subseq contents 0 (1+ i))))
            (setf 
             (aref sdoc-contents i) (aref match-strings 0)
             (aref contents i) (concatenate 'string (aref match-strings 0) (aref match-strings 1)))
            (doc-push-sdoc-chunk doc (make-doc-chunk schema sdoc-contents))
            (return-from extract-sdoc contents))))))

(defun get-long-block (text doc)
  (let* ((schema (line-pragma text))
        (contents (make-svector (line-text text)))
        (check-sdoc (and (not (doc-sdoc doc)) (not schema))))
    (do ((p (cdr text) (cdr p)))
        ((or (not p) (line-pragma p))
         (if check-sdoc
             (setf contents (extract-sdoc schema contents doc)))
         (if (plusp (length contents))
             (doc-push-ldoc-chunk doc (make-doc-chunk schema contents)))
        p)
      (if (and check-sdoc (blank-line p))
          (setf contents (extract-sdoc schema contents doc) check-sdoc nil))
      (vector-push-extend (line-text p) contents))))

(defun compile-raw-doc (text)
  (let ((doc (make-doc)))
    (do ((text (sift-pragmas text)))
        ((not text) doc)
      (case (line-pragma text)
        ((:namespace :type :const :enum) 
         (setf 
          (doc-type doc) (line-pragma text) 
          text (cdr text)))

        (:escape
         (setf text (cdr text)))

        ((:n :note :w :warn :c :code)
         (setf text (get-simple-chunk text doc)))

        ((:r :returns :t :throws)
         (setf text (get-return-or-throw-block text doc)))

        ((:todo :todoc :in :inote)
         (setf text (get-inote-block text doc)))
        
        (t
         (setf text (get-long-block text doc)))))))

(defun create-*-doc (
  ast     ;the ast to document
  raw-doc ;the raw documentation associated with this ast that was attached to a node higher in the tree
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
  (dump-doc-items *standard-output*)
)
#|

;;for quick testing
(defun dump-doc-itemsx () 
  (maphash (lambda (key value) (format t "~A~%~A~%~%" key value)) 
           *doc-items*))


(defun dump-doc-itemxx (key value)
  (with-xml-output (*standard-output*)
    (xml-emitter:with-tag ("person" (list (list "age" (+ 1 19))))
      (xml-emitter:with-simple-tag ("firstName")
        (xml-emitter:xml-out "Peter"))
      (xml-emitter:simple-tag "lastName" "Scott")
      (xml-emitter:emit-simple-tags :age 17
                                    :school "Iowa State Univeristy"
                                    "mixedCaseTag" "Check out the mixed case!"
                                    "notShown" nil))))

(defun dump-doc-item0 (k v)
  (format t "~A~%~A~%~%" k v))

(defun dump-doc-item (k v)
;  (dump-doc-item0 k v)
  ;(with-xml-output (*standard-output*)
    (xml-emitter:with-tag ((doc-type v) (list (list "name" k)))
      (xml-emitter:simple-tag "sdoc" (doc-sdoc v))
      )
   ; )
  )


(defun dump-doc-items ()
  (maphash #'dump-doc-item *doc-items*))

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