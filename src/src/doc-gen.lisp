(in-package #:js-proc)

(defun make-svector (&optional (first-value nil))
  (let ((result (make-array 100 :element-type 'string :fill-pointer 0 :adjustable t)))
    (if first-value
        (vector-push-extend first-value result))
    result))

;
; take a list of text and sift it into a list sifted lines
;

(defparameter *pragma-map*
  (let ((pragmas (make-hash-table :test 'equal)))
    (dolist (word '(:mu :md :namespace :const :enum :type :variable :note :warn :code :return :throw :case :result :todo :todoc :inote))
      (setf (gethash (string-downcase (string word)) pragmas) word))
    (setf 
     (gethash "/" pragmas) :end

     ;synonms...
     (gethash "n" pragmas) :note
     (gethash "w" pragmas) :warn
     (gethash "r" pragmas) :return
     (gethash "t" pragmas) :throw
     (gethash "c" pragmas) :code
     (gethash ">" pragmas) :case
     (gethash "<" pragmas) :result
     (gethash "in" pragmas) :inote
     )
    pragmas))

(defun map-basic-pragmas (pragma args)
  (declare (ignore args))
  (let ((pragma (gethash pragma *pragma-map*)))
    (case pragma
      ((:mu :md :note :warn :code :todo :todoc :inote :end) pragma)
      (t nil))))


(defun map-object-pragmas (pragma args)
  (declare (ignore args))
  (let ((pragma (gethash pragma *pragma-map*)))
    (case pragma
      ((:mu :md :note :warn :code :todo :todoc :inote :end :namespace :const :enum :type :variable) pragma)
      (t nil))))

(defun map-function-pragmas (pragma args)
  (declare (ignore args))
  (let ((pragma (gethash pragma *pragma-map*)))
    (case pragma
      ((:mu :md :note :warn :code :todo :todoc :inote :end :return :throw :case :result) pragma)
      (t nil))))

(defun map-parameter-pragmas (pragma args)
  (if (and (not pragma) args)
      :paramType
      (map-basic-pragmas pragma args)))

(defstruct sifted-line
  pragma ;the pragma on the line; nil if none
  args   ;the pragma arguments on the line; nil if none
  text   ;anything and everything after the pragma and args
)

(defparameter pragma-scanner 
  ; ^((//)(\\s*`)?|(\\s*`))  start of line followed by "//" or "//<spaces>`" or "<spaces>`"
  ; ([^\\s\\(]+)? -- the pragma; one or more of anything other than a space or left parentheses
  ; (\\([^\\)]*\\))? -- the pragma arguments; anything other than a right parentheses enclosed in parentheses.
  ; (\\s+(.*))? everything after the pragma; must have at least one space after the pragma
  ;
  ; notice that it is possible to match with both nil pragma and pragma arguments (e.g., "// the rest")
  ; therefore, a match is known when match is true and one or both of the pragma/pragma arguments is non-nil
  (cl-ppcre:create-scanner "^((//)(\\s*`)?|(\\s*`))([^\\s\\(]+)?(\\(([^\\)]*)\\))?(\\s+.*)?"))
  ;                          01   2        3       4            5   6             7    

(defparameter //-comment-scanner 
  (cl-ppcre:create-scanner "^//"))

(defparameter non-space-scanner 
  (cl-ppcre:create-scanner "\\S"))

(defun sift-pragmas (text pragma-map)
  ; map text to a list of sifted-lines
  ; nil pragma implies there was no pragma as given by *pragmas*
  ; text is modified by replacing the pragma positions with spaces, right-trimming, and deleting any leading "//"
  ; if text contain only spaces (after modification, then text is set to nil
  (map 'list 
       (lambda (s)
         (multiple-value-bind (match match-strings) (cl-ppcre:scan-to-strings pragma-scanner s)
           (let* ((pragma (and match (funcall pragma-map (aref match-strings 4) (aref match-strings 6))))
                  (rest-of-line (and pragma (aref match-strings 7) (string-right-trim '(#\Space #\Tab) (aref match-strings 7))))
                  (spaces (and rest-of-line (make-string (do ;the sum of the lengths of match-strings 2, 3, 4, 5
                                                          ((sum 0)
                                                           (i 2 (incf i)))
                                                          ((> i 5)
                                                           sum)
                                                           (setf sum (+ sum (length (aref match-strings i))))) :initial-element #\space))))
             (if pragma
                 (make-sifted-line :pragma pragma :args (aref match-strings 6) :text (and rest-of-line (concatenate 'string spaces rest-of-line)))
                 (let ((s (cl-ppcre:regex-replace //-comment-scanner s "")))
                     (make-sifted-line :text (and (cl-ppcre:scan non-space-scanner s) (string-right-trim '(#\Space #\Tab) s))))))))
             text))
 
(defun line-pragma (line)
  (and line (sifted-line-pragma (car line))))

(defun line-pragma-args (line)
  (and line (sifted-line-args (car line))))

(defun line-text (line)
  (and line (sifted-line-text (car line))))

(defparameter sdoc-scanner 
  ;notice this is non-greedy for the chunk before the //
  (cl-ppcre:create-scanner "(.*?)//(.*)" :single-line-mode t))
  ;                         0      1

(defun fixup-sdoc (doc)
  (if (not (doc-sdoc doc))
      (let ((candidate (find-if (lambda (chunk) (eq (doc-chunk-schema chunk) :md)) (doc-ldoc doc))))
        (if candidate
            (multiple-value-bind (match match-strings) (cl-ppcre:scan-to-strings sdoc-scanner (doc-chunk-text candidate))
              (if match
                  (setf
                   (doc-sdoc doc) (doc-section-push-chunk (make-doc-chunk :md (aref match-strings 0) nil))
                   (doc-chunk-text candidate) (concatenate 'string (aref match-strings 0) (aref match-strings 1)))
                  (setf
                   (doc-sdoc doc) (doc-section-push-chunk candidate)
                   (doc-ldoc doc) (remove candidate (doc-ldoc doc)))))))))

(defun get-doc-chunk (pragma text section)
  (do ((contents (make-svector (line-text text)) )
        (p (cdr text) (cdr p)))
      ((or (not p) (line-pragma p))
       (let ((chunk (make-doc-chunk pragma contents)))
         (if (plusp (length (doc-chunk-text chunk)))
             (vector-push-extend chunk section)))
       p)
    (vector-push-extend (line-text p) contents)))

(defun blank-line (line)
  (not (line-text line)))

(defun get-doc-simple-subsection (pragma text section)
  (let ((end 
         ;if the next pragma line is an end or eof, then that line; 
         ;othewise any other pragma or the first blank line, whichever comes first
         (do ((first-blank-line nil)
              (p (cdr text) (cdr p)))
             ((or (not p) (line-pragma p)) 
              (if (or (not p) (and p (eq (line-pragma p) :end)))
                  p
                  (or first-blank-line p)))
           (if (and (not first-blank-line) (blank-line p))
               (setf first-blank-line p)))))
    (do ((contents (make-svector (line-text text)) )
         (p (cdr text) (cdr p)))
        ((eq p end)
         (vector-push-extend (make-doc-chunk pragma contents) section)
         p)
      (vector-push-extend (line-text p) contents))))

(defun get-doc-return/throw-subsection (pragma text section)
  ;TODO
  (declare (ignore pragma text section))
)

(defun get-doc-param-type-section (text param)
  ;text is a sifted list of pragmas; the first pragma should be a :paramType
  ;process until another :paramType or end is encountered

  ;assume the documentation block for a parameter always starts with a :paramType pragma...
  (if (not (eq (line-pragma text) :paramType))
      (format t "ERROR: A parameter documentation block started with some pragma other than a parameter type; replaced pragma with parameter type pragma.~%~A~%" (line-text text)))
  (if (not (line-pragma-args text))
      (format t "ERROR: A parameter documentation block started without a type specification; replaced type specification with \"()\".~%~A~%" (line-text text)))

  (let ((type (line-pragma-args text))
        (section (make-doc-section)))
    (do* ((p text)
          (pragma :md (line-pragma p)))
         ((or (not p) (eq pragma :paramType))
          (doc-param-push-type param type section)
          p)
      (case pragma
        ((:note :warn :code)
         (setf p (get-doc-simple-subsection pragma p section)))

        (:end
         (setf p (cdr p)))

        (t
         (setf p (get-doc-chunk (or pragma :md) p section)))))))

(defun get-doc-param (name raw-doc)
  (let ((param (make-doc-param name)))
    (do ((text 
          (sift-pragmas raw-doc #'map-parameter-pragmas) 
          (get-doc-param-type-section text param)))
        ((not text)
         param))))

(defun get-doc-params (param-list)
  ;param-list is a list of (name . comment) pairs
  (let ((params (make-doc-params)))
    (dolist (p param-list params)
      (vector-push-extend (get-doc-param (car p) (cdr p)) params))))

(defun create-*-doc (
  ast     ;the ast to document
  raw-doc ;the raw documentation associated with ast
)
  (case (get-ast-type ast)
    (:function 
     (create-function-doc ast raw-doc))
    (:object 
     (create-object-doc ast raw-doc))
    (:array 
     (create-array-doc ast raw-doc))
    (t
     (create-variable-doc ast raw-doc))))

(defun create-variable-doc (
  ast ;an expression ast that's not an array, object, or function literal
  raw-doc ;the raw documentation assocated with ast
)
  (declare (ignore ast))
  (let ((doc (make-doc :type :variable)))
    (do* ((text (sift-pragmas raw-doc #'map-object-pragmas))
          (pragma (line-pragma text) (line-pragma text)))
        ((not text))
      (case pragma
        ((:const :variable) 
         (setf 
          (doc-type doc) pragma 
          text (cdr text)))

        ((:note :warn :code)
         (setf text (get-doc-simple-subsection pragma text (doc-get-ldoc doc))))

        ((:todo :todoc :inote)
         (setf text (get-doc-simple-subsection pragma text (doc-get-inotes doc))))
        
        (:end
         (setf text (cdr text)))

        (t
         (setf text (get-doc-chunk (or pragma :md) text (doc-get-ldoc doc))))))

    (fixup-sdoc doc)
    doc))

(defun create-array-doc (
  ast ;ast for an array literal
  raw-doc ;the raw documentation assocated with ast
)
  (declare (ignore ast))
  (let ((doc (make-doc :type :variable)))
    (do* ((text (sift-pragmas raw-doc #'map-object-pragmas))
          (pragma (line-pragma text) (line-pragma text)))
        ((not text))
      (case pragma
        ((:const :variable) 
         (setf 
          (doc-type doc) pragma 
          text (cdr text)))

        ((:note :warn :code)
         (setf text (get-doc-simple-subsection pragma text (doc-get-ldoc doc))))

        ((:todo :todoc :inote)
         (setf text (get-doc-simple-subsection pragma text (doc-get-inotes doc))))
        
        (:end
         (setf text (cdr text)))

        (t
         (setf text (get-doc-chunk (or pragma :md) text (doc-get-ldoc doc))))))

    (fixup-sdoc doc)

    doc))

(defun create-object-doc (
  ast ;ast for an object literal
  raw-doc ;the raw documentation assocated with ast
)
  (let ((doc (make-doc :type :variable)))
    (do* ((text (sift-pragmas raw-doc #'map-object-pragmas))
          (pragma (line-pragma text) (line-pragma text)))
        ((not text))
      (case pragma
        ((:namespace :type :const :enum :variable) 
         (setf 
          (doc-type doc) pragma 
          text (cdr text)))

        ((:note :warn :code)
         (setf text (get-doc-simple-subsection pragma text (doc-get-ldoc doc))))

        ((:todo :todoc :inote)
         (setf text (get-doc-simple-subsection pragma text (doc-get-inotes doc))))
        
        (:end
         (setf text (cdr text)))

        (t
         (setf text (get-doc-chunk (or pragma :md) text (doc-get-ldoc doc))))))

    (fixup-sdoc doc)

    (setf (doc-members doc)
          (let ((members (make-hash-table :test 'equal)))
            (dolist (member (second ast) members)
              (let* ((name (first member))
                     (ast (third member))
                     (raw-doc (or (second member) (get-ast-comment ast))))
                (setf (gethash name members) (create-*-doc ast raw-doc))))))
    doc))

(defun create-function-doc (
  ast ;ast for a function literal
  raw-doc ;the raw documentation assocated with ast
)
  (let ((doc (make-doc :type :function)))
    (do* ((text (sift-pragmas raw-doc #'map-function-pragmas))
          (pragma (line-pragma text) (line-pragma text)))
        ((not text))
      (case pragma
        (:return
         (setf text (get-doc-return/throw-subsection pragma text (doc-get-returns doc))))

        (:throw
         (setf text (get-doc-return/throw-subsection pragma text (doc-get-throws doc))))

        ((:note :warn :code)
         (setf text (get-doc-simple-subsection pragma text (doc-get-ldoc doc))))

        ((:todo :todoc :inote)
         (setf text (get-doc-simple-subsection pragma text (doc-get-inotes doc))))
        
        (:end
         (setf text (cdr text)))

        (t
         (setf text (get-doc-chunk (or pragma :md) text (doc-get-ldoc doc))))))

    (fixup-sdoc doc)

    ;third of ast is a list of (name . comment) pairs, one for each parameter
    (if (third ast)
        (setf (doc-params doc) (get-doc-params (third ast))))
    doc))

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
         (rhs (get-ast-assign-attrib ast :right))
         (raw-doc (or (get-ast-comment ast) (get-ast-comment rhs)))
         (doc (and name raw-doc (create-*-doc rhs raw-doc))))
    (if doc
        (progn
          (setf (doc-location doc) (get-ast-location ast))
          (append-doc name doc)))))

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
