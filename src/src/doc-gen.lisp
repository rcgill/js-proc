(in-package #:js-proc)

#|
TODO:
// bla
///
//return 
// etc

fails

If you put an overload in a function that doesn't have a doc comment (or maybe params) fails.

if a module doesn't have a doc block--seem to crach
|#

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
    (dolist (word '(:mu :md :namespace :const :enum :type :variable :kwargs :hash :private :nosource :note :warn :code :return :throw :case :todo :todoc :inote :file :require :classattr))
      (let ((word-string (string-downcase (string word))))
        (setf 
         (gethash word-string pragmas) word
         (gethash (concatenate 'string "/" word-string) pragmas) word)))
    (setf 
     (gethash "/" pragmas) :end

     ;synonms...
     (gethash "n" pragmas) :note
     (gethash "w" pragmas) :warn
     (gethash "r" pragmas) :return
     (gethash "t" pragmas) :throw
     (gethash "c" pragmas) :code
     (gethash ">" pragmas) :case
     (gethash "in" pragmas) :inote
     (gethash "/n" pragmas) :note
     (gethash "/w" pragmas) :warn
     (gethash "/r" pragmas) :return
     (gethash "/t" pragmas) :throw
     (gethash "/c" pragmas) :code
     (gethash "/>" pragmas) :case
     (gethash "/in" pragmas) :inote
     )
    pragmas))

(defun check-pragma (pragma args)
  (let ((pragma (gethash pragma *pragma-map*)))
    (if (and (or (eq pragma :end) (not pragma)) args) 
        :paramType
        pragma)))

(defstruct sifted-line
  pragma ;the pragma on the line; nil if none
  args   ;the pragma arguments on the line; nil if none
  text   ;anything and everything after the pragma and args
)

(defparameter pragma-scanner 
  ; ^((//)(\\s*`)?|(\\s*`))  start of line followed by "//" or "//<spaces>`" or "<spaces>`"
  ; ([^\\s\\(]+)? -- the pragma; one or more of anything other than a space or left parentheses
  
  ; (\\([^\\)]*\\))? -- the pragma arguments; anything other than a right parentheses enclosed in parentheses.
  ; (\\((.*))? -- the pragma arguments; anything other than a right parentheses enclosed in parentheses.
  
   ; (\\s+(.*))? everything after the pragma; must have at least one space after the pragma
  ;
  ; notice that it is possible to match with both nil pragma and pragma arguments (e.g., "// the rest")
  ; therefore, a match is known when match is true and one or both of the pragma/pragma arguments is non-nil
  ;(cl-ppcre:create-scanner "^((//)(\\s*`)?|(\\s*`))([^\\s\\(]+)?(\\(([^\\)]*)\\))?(\\s+.*)?"))
  ;;                          01   2        3       4            5   6             7    
  (cl-ppcre:create-scanner "^((//)(\\s*`)?|(\\s*`))([^\\s\\(]+)?(\\((.*))?(\\s+.*)?"))
  ;                          01   2        3       4            5   6             7    


(defparameter include-scanner
  (cl-ppcre:create-scanner "^\\s*//include\\s+(.+)"))

(defparameter //-comment-scanner 
  (cl-ppcre:create-scanner "^//"))

(defparameter non-space-scanner 
  (cl-ppcre:create-scanner "\\S"))

(defun get-args (args rest-of-line)
  (if args
      (let* ((left-parent-count 1)
             (length-of-args (do ((i 0))
                                 ((or (>= i (length args)) (zerop left-parent-count)) i)
                               (case (char args i)
                                 (#\) (decf left-parent-count))
                                 (#\( (incf left-parent-count)))
                               (if (plusp left-parent-count)
                                   (incf i)))))
        (if (zerop left-parent-count)
            (values 
             (subseq args 0 length-of-args) 
             (if (eql (1+ length-of-args) (length args)) 
               nil
               (string-trim '(#\Space #\Tab) (subseq args (1+ length-of-args)))))
            (values nil args)))
      (values nil rest-of-line)))

(defun trimmed-text (text)
  (let ((text (and (plusp (length text)) (string-right-trim '(#\Space #\Tab) text))))
    (and (plusp (length text)) text)))

(defun sift-pragmas (text)
  ; map text to a list of sifted-lines
  ; nil pragma implies there was no pragma as given by *pragmas*
  ; text is modified by replacing the pragma positions with spaces, right-trimming, and deleting any leading "//"
  ; if text contain only spaces (after modification, then text is set to nil
  (map 'list 
       (lambda (s)
         (multiple-value-bind (match match-strings) (cl-ppcre:scan-to-strings pragma-scanner s)
           (multiple-value-bind (args rest-of-line) (get-args (aref match-strings 6) (aref match-strings 7))
             (let ((pragma (and match (check-pragma (aref match-strings 4) args))))
               (if pragma
                   (make-sifted-line :pragma pragma :args args :text (trimmed-text rest-of-line))
                   (let ((s (cl-ppcre:regex-replace //-comment-scanner s "")))
                     (make-sifted-line :text (and (cl-ppcre:scan non-space-scanner s) (string-right-trim '(#\Space #\Tab) s)))))))))
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
      (let ((candidate (find-if (lambda (chunk) (eq (doc-chunk-pragma chunk) :md)) (doc-ldoc doc))))
        (if candidate
            (multiple-value-bind (match match-strings) (cl-ppcre:scan-to-strings sdoc-scanner (doc-chunk-text candidate))
              (if match
                  (setf
                   (doc-sdoc doc) (doc-section-push-chunk (make-doc-chunk :md (aref match-strings 0) nil))
                   (doc-chunk-text candidate) (concatenate 'string (aref match-strings 0) (aref match-strings 1)))
                  (setf
                   (doc-sdoc doc) (doc-section-push-chunk candidate)
                   (doc-ldoc doc) (remove candidate (doc-ldoc doc))))))))
  doc)

(defun get-doc-chunk (pragma text section)
  (do ((contents (make-svector (line-text text)))
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

(defun get-doc-return/throw-subsection (text doc push-function)
  (do ()
      ((or (not text) (not (eq (line-pragma text) :paramType)))
       text)
    (multiple-value-bind 
          (type section next) (get-doc-param-type-section text)
      (funcall push-function doc type section)
      (setf text next))))

(defun get-doc-param-type-section (text)
  ;text is a sifted list of pragmas; the first pragma should be a :paramType
  ;process until another :paramType, :end, or end is encountered
  (let ((type (line-pragma-args text))
        (section (make-doc-section)))
    (do* ((p text)
          (pragma :md (line-pragma p)))
         ((or (not p) (eq pragma :paramType) (eq pragma :end))
          (values type section p))
      (case pragma
        ((:note :warn :code :case)
         (setf p (get-doc-simple-subsection pragma p section)))

        (:end
         (if (line-text p) 
             (setf p (get-doc-chunk :md p section))    
             (setf p (cdr p))))
        (t
         (setf p (get-doc-chunk (or pragma :md) p section)))))))

(defun get-doc-param (name raw-doc)
  (let ((param (make-doc-param name))
        (general-section (make-doc-section)))
    (do* ((text (sift-pragmas raw-doc))
         (pragma (line-pragma text) (line-pragma text)))
        ((not text) 
         (if (plusp (length general-section))
             (doc-param-push-type param nil general-section))
         param)
      (case pragma
        (:paramType
         (multiple-value-bind (type section next) (get-doc-param-type-section text)
                           (doc-param-push-type param type section)
                           (setf text next)))

        ((:note :warn :code :todo :todoc :inote)
         (setf text (get-doc-simple-subsection pragma text general-section)))

        (:end
         (if (line-text text) 
             (setf text (get-doc-chunk :md text general-section))    
             (setf text (cdr text))))
                 
        (t
         (setf text (get-doc-chunk (or pragma :md) text general-section)))))))

(defun get-doc-params (param-list ensure-doc-when-comment-exists)
  ;param-list is a list of (name . comment) pairs
  (if param-list
      (let ((params (make-doc-params)))
        (dolist (p param-list params)
          (funcall ensure-doc-when-comment-exists (cdr p))
          (vector-push-extend (get-doc-param (car p) (first (cdr p))) params)))))

(defun gen-doc (comment)
  (cond
    ((not comment) nil)

    ((sixth comment) t)

    (t (let ((line (aref (first comment) 0)))
         (and (>= (length line) 3) (equal (subseq line 0 3) "///"))))))

(defun create-*-doc (raw-doc type)
  ;raw-doc is a vector of strings; create iff raw-doc[0][0..2]==="///"
  (if (gen-doc raw-doc)
      (let ((doc (make-doc :type type)))
        (do* ((text (sift-pragmas (first raw-doc)))
              (pragma (line-pragma text) (line-pragma text)))
             ((not text))
          (case pragma
            ((:namespace :type :const :enum :variable) 
             (setf 
              (doc-type doc) pragma 
              text (cdr text)))

            ((:kwargs :hash :nosource :private :classattr)
             (push pragma (doc-flags doc))
             (setf text (cdr text)))
           
            ((:note :warn :code)
             (setf text (get-doc-simple-subsection pragma text (doc-get-ldoc doc))))

            (:paramType ;for a object property
             (multiple-value-bind (type section next) (get-doc-param-type-section text)
               (doc-types-push-type doc type section)
               (setf text next)))
            
            ((:todo :todoc :inote)
             (setf text (get-doc-simple-subsection pragma text (doc-get-inotes doc))))
            
            (:return
              ;TODO--this is sloppy--we demand that a return be on an empty line
              (setf text (cdr text))
              (setf text (get-doc-return/throw-subsection text doc #'doc-push-return)))
            
            (:throw
                ;TODO--this is sloppy--we demand that a throw be on an empty line
                (setf text (cdr text))
                (setf text (get-doc-return/throw-subsection text doc #'doc-push-throw)))
            
            (:end
             (if (line-text text) 
                 (setf text (get-doc-chunk :md text (doc-get-ldoc doc)))    
                 (setf text (cdr text))))
            
            (t
             (setf text (get-doc-chunk (or pragma :md) text (doc-get-ldoc doc))))))
        (fixup-sdoc doc))))

(defun create-doc (ast)
  (setf (asn-doc ast) (create-*-doc (asn-comment ast) :variable)))

(defun create-function-doc (ast ensure-doc-when-comment-exists)
  (let ((doc (create-*-doc (asn-comment ast) :function)))
    (if doc
        (progn
          (if (not (eq (doc-type doc) :function)) 
              (push :function (doc-flags doc)))
          (setf 
           (doc-params doc) (get-doc-params (second (asn-children ast)) ensure-doc-when-comment-exists)
           (doc-location doc) (asn-location ast)
           (asn-doc ast) doc)))))

(defun process-return (ast current-doc)
  (if (and (gen-doc (asn-comment ast)) current-doc)
      (do ((text (sift-pragmas (first (asn-comment ast)))))
          ((not text))
        (multiple-value-bind 
              (type section next) (get-doc-param-type-section text)
          (doc-push-return current-doc type section)
          (setf text next)))))

(defun process-comment-island (
  raw-doc ;the raw documentation
  resource ;the resource that contained the raw-doc
  current-doc-item ;the current documentable item being processed
)
#|
TODO the comment island

/*
  var element= document.createElement("script");
  element.src= src;
  element.type= type || "text/javascript";
  element.charset= charset || "utf-8";
  //element.defer= "defer";
  document.getElementsByTagName("head")[0].appendChild(element);
};
*/

bombs out the regex

|#
  (if (gen-doc (token-value raw-doc))
      (let ((text (sift-pragmas (first (token-value raw-doc))))
            doc)
        (if (eq (line-pragma text) :file)
            (setf doc (resource-doc resource)
                  (sifted-line-pragma (car text)) :md)
            (setf doc current-doc-item))
        (do ((pragma (line-pragma text) (line-pragma text)))
            ((not text))
          (case pragma
            ((:note :warn :code)
             (setf text (get-doc-simple-subsection pragma text (doc-get-ldoc doc))))

            (:require
             (doc-push-require doc (line-text text))
             (setf text (cdr text)))
            
            ((:todo :todoc :inote)
             (setf text (get-doc-simple-subsection pragma text (doc-get-inotes doc))))

            (:end
             (if (line-text text) 
                 (setf text (get-doc-chunk :md text (doc-get-ldoc doc)))    
                 (setf text (cdr text))))
            
            (t
             (setf text (get-doc-chunk (or pragma :md) text (doc-get-ldoc doc))))))
        (fixup-sdoc doc))))

;;
;; These are the special processing functions
;;

(defun process-dojo-mixin (arg-list get-doc-item append-doc-item)
  (let* ((parent-name (get-ast-name (first arg-list)))
         (parent (or 
                   (funcall get-doc-item parent-name :namespace)
                   (funcall get-doc-item parent-name :variable)
                   (funcall get-doc-item parent-name :function))))
    (if parent
        (let* ((arg2 (second arg-list))
               (children (and (eq (asn-type arg2) :object) (asn-children arg2))))
          (dolist (prop children)
            ;prop is (name . expr), both are asn's
            (let* ((prop-name (car prop))
                   (prop-value (cdr prop))
                   (prop-doc (or (asn-doc prop-name) (asn-doc prop-value))))
              (if prop-doc
                  (progn
                    (setf (doc-location prop-doc) (sum-locations (asn-location prop-name)(asn-location prop-value)))
                    (funcall append-doc-item (concatenate 'string parent-name "." (get-ast-name prop-name)) prop-doc)))))))))

(defun process-dojo-provide (args resource)
  ;args is the list of argument expressions sent to dojo.provide
  ;we only pay attention if it a :string asn
  (if (eq (asn-type (first args)) :string)
      (let ((doc (resource-doc resource)))
        (push (token-value (asn-children (first args))) (doc-provides doc)))))

(defun process-dojo-require (args resource)
  ;args is the list of argument expressions sent to dojo.require
  ;we only pay attention if it a :string asn
  ;TODO need to make this better to handle a vector of requires
  (if (eq (asn-type (first args)) :string)
      (let ((doc (resource-doc resource)))
        (push (token-value (asn-children (first args))) (doc-requires doc)))))

(defun process-dojo-def (args resource loc append-doc-item)
  ;args is the list of argument expressions sent to dojo.def
  ;we only pay attention if it the first argument is a :string
  (if (eq (asn-type (first args)) :string)
      (let ((name (token-value (asn-children (first args))))
            (doc (asn-doc (or (third args) (second args)))))
        (push :module (doc-flags doc))
        (setf 
         (doc-location doc) loc
         (doc-type doc) :module)
        (funcall append-doc-item name doc)
        (push name (doc-modules (resource-doc resource))))))

(defparameter colon-scanner
  (cl-ppcre:create-scanner ":"))

(defun process-bd-declare (ast arg-list append-doc-item)
  ;args is the list of argument expressions sent to bd.declare
  (let ((class-name (cl-ppcre:regex-replace colon-scanner (token-value (asn-children (first arg-list))) "."))
        (supers (second arg-list))
        (doc (asn-doc ast))
        (loc (asn-location ast)))
    (setf
     (doc-type doc) :class
     (doc-location doc) loc)
    (case (asn-type supers)
      (:array 
       (setf (doc-supers doc) (mapcar #'get-ast-name (asn-children supers))))
      ((:name :dot)
       (setf (doc-supers doc) (cons (get-ast-name supers) nil)))
      (t
       (setf (doc-supers doc) nil)))
    (dolist (arg (cdr (cdr arg-list)))
      (if (eq (asn-type arg) :object)
          (setf (doc-members doc) (nconc (doc-members doc) (doc-properties (asn-doc arg))))
          (if (equal (get-ast-name (car (asn-children arg))) "bd.makeDeferredConnects")
              (let ((deferred-connects-doc (asn-doc arg)))
                (setf (doc-location deferred-connects-doc) (asn-location arg))
                (setf (doc-members doc) (nconc (list (cons "deferredConnects" deferred-connects-doc)) (doc-members doc))))
            (let ((name (token-value (asn-children (second (asn-children arg)))))
                  (attr-doc (asn-doc arg)))
              (setf (doc-location attr-doc) (asn-location arg))
              (setf (doc-members doc) (nconc (list (cons name attr-doc)) (doc-members doc)))))))
    (funcall append-doc-item class-name doc)))

(defun process-dojo-declare (loc arg-list append-doc-item)
  ;args is the list of argument expressions sent to dojo.declare
  (let ((class-name (concatenate 'string (get-ast-name (second arg-list)) "." (token-value (asn-children (first arg-list)))))
        (supers (third arg-list))
        (doc (asn-doc (fourth arg-list))))
    (setf 
     (doc-type doc) :class
     (doc-location doc) loc
     (doc-members doc) (doc-properties doc)
     (doc-properties doc) nil)

    (case (asn-type supers)
      (:array 
       (setf (doc-supers doc) (mapcar #'get-ast-name (asn-children supers))))
      ((:name :dot)
        (setf (doc-supers doc) (cons (get-ast-name supers) nil)))
      (t
       (setf (doc-supers doc) nil)))

    (funcall append-doc-item class-name doc)))

(defun process-bd-doc-group-overload (args doc)
  (do ((result nil)
       (p args (cdr p)))
      ((or (not p) (not (eq (asn-type (first p)) :function-literal))) 
       (setf (doc-overloads doc) (reverse result))
       p)
    (push (asn-doc (first p)) result)))
  
(defun process-bd-doc-group-prefix (prefix object append-doc-item)
  (dolist (item (asn-children object))
    ;item is a cons (name . value)
    (let* ((name (car item))
           (value (cdr item))
           (doc (or (asn-doc name) (asn-doc value))))
      (if (eq (doc-type doc) :function) 
          (push :function (doc-flags doc)))
      (setf (doc-location doc) (sum-locations (asn-location name)(asn-location value)))
      (funcall append-doc-item (concatenate 'string prefix (token-value (asn-children name))) doc))))

(defun process-bd-doc (args append-doc-item bd-doc-asn)
  ;args is the list of argument expressions sent to bd.docGen
  ;the expressions come in one or more groups of:

  ; "overload", <function-literal>
  ; appears as first expression in function and gives possible overload signatures for function.
  ; this will be processed by the function doc generator

  ; "kwargs", <object-literal>
  ; appears as first expression in function and gives kwargs for function

  ; "<any non-empty-string>", <object-literal>
  ; appears anywhere, defined names are given by <string-value>.<property-value>

  ; <object-literal> 
  ; appears anywhere, defined names are given by <property value>

  (if (not (asn-doc bd-doc-asn))
      (setf (asn-doc bd-doc-asn) (make-doc)))

  (do ((p args)
       (doc (asn-doc bd-doc-asn)))
      ((not p))
    (let ((prefix (and (eq (asn-type (first p)) :string) (token-value (asn-children (first p))))))
      (cond
        ((equal prefix "overload")
         (setf p (process-bd-doc-group-overload (cdr p) doc)))

        ((equal prefix "kwargs")
         (setf (doc-kwargs doc) (asn-doc (second p))
               p (third p)))

         (prefix
          (process-bd-doc-group-prefix (concatenate 'string prefix ".") (second p) append-doc-item)
          (setf p (third p)))

         (t
          (process-bd-doc-group-prefix "" p append-doc-item)
          (setf p (second p)))))))

;;
;; These functions decode an ast node
;;
(defun get-ast-name (ast)
  (case (asn-type ast)
    (:name
     (token-value (asn-children ast)))
    (:dot
     (concatenate 'string 
                  (get-ast-name (car (asn-children ast))) 
                  "." 
                  (get-ast-name (cdr (asn-children ast)))))
    (t nil)))

(defun get-asn-name-root (ast)
  (let ((name
        (case (asn-type ast)
          (:name
           (token-value (asn-children ast)))
          (:dot
           (get-asn-name-root (car (asn-children ast))))
          (t nil))))
    (and (not (equal name "this")) name)))

(defparameter kwargs
  (cl-ppcre:create-scanner "\.kwargs$"))

(defun doc-gen (resource append-doc-item get-doc-item)
  (setf (resource-doc resource) (funcall get-doc-item (resource-name resource) :resource t))
  (let ((doc-stack (list (resource-doc resource)))
        (source-text (resource-text resource)))
    (labels ((ensure-doc-when-comment-exists (comment)
               ;;if comment, then signal that this comment should generate a doc item by setting (fifth comment) = t
               ;;also ensure source starts with "///"
               (if comment
                   (let* ((comment-vector (first comment))
                          (line1 (aref comment-vector 0)))
                     (if (or (< (length line1) 3) (not (equal (subseq line1 0 3) "///")))
                         ;got a comment that does not begin with "///" (note: it should begin with "//")
                         (let* ((source-line-index (second comment))
                                (source-char-index (third comment))
                                (prefix (subseq (aref source-text source-line-index) 0 source-char-index)))
                           (setf (aref source-text source-line-index) (concatenate 'string prefix "/" line1))
                           (nconc comment '(t)))))))

             (traverse (ast append-doc-item)
               
               (if (not ast)
                   (return-from traverse))
               (case (asn-type ast)
                 ((:root :block) 
                  ;children --> list of statements
                  (dolist (statement (asn-children ast))
                    (traverse statement append-doc-item)))

                 (:comment
                  ;children --> comment-token
                  (process-comment-island (asn-children ast) resource (first doc-stack)))

                 (:label
                  ;children --> (label . statement)
                  (traverse (second (asn-children ast)) append-doc-item))
                 
                 (:switch
                  ;children --> (switch-expr  . case-list)
                  (let ((children (asn-children ast)))
                    (traverse (car children)  append-doc-item)
                    (dolist (case-item (cdr children))
                      (traverse case-item  append-doc-item))))

                 (:case
                  ;children --> expression
                  (traverse (asn-children ast) append-doc-item))

                 (:default
                  ;children --> nil
                  )

                 (:debugger
                  ;children --> nil
                  )

                 (:do
                  ;children --> (condition . statment)
                  (let ((children (asn-children ast)))
                    (traverse (car children) append-doc-item)
                    (traverse (cdr children) append-doc-item)))

                 (:return
                  ;children --> expression
                   (if (asn-comment ast)
                       (process-return ast (first doc-stack)))
                   (traverse (asn-children ast) append-doc-item))

                 (:throw
                  ;children --> throw-expr
                  (if (asn-comment ast)
                      ;todo---push the comment into the current function
                      nil)
                  (traverse (asn-children ast) append-doc-item))

                 (:var
                  ;children --> vardefs
                  ;vardefs is a list of lexical-var (name, init-val(expr), comment)
                  ;name is a token, expr is an expression or nil
                  (dolist (def (asn-children ast))
                    (if (and (gen-doc (lexical-var-comment def)) )
                        (funcall append-doc-item (token-value (lexical-var-name def)) (create-*-doc (lexical-var-comment def) :variable)))
;;TODO...DELETE                    (and (lexical-var-init-val def) (traverse (lexical-var-init-val def) append-doc-item))
))

                 (:while
                  ;children --> (while-condition . while-statement)
                  (let ((children (asn-children ast)))
                    (traverse (car children) append-doc-item)
                    (traverse (cdr children) append-doc-item)))

                 (:with
                  ;children --> (with-expr . with-statement)
                  (let ((children (asn-children ast)))
                    (traverse (car children) append-doc-item)
                    (traverse (cdr children) append-doc-item)))

                 (:statement
                  ;children --> expression
                  (traverse (asn-children ast) append-doc-item))

                 ((:break :continue)
                  ;children --> name-token
                  )

                 (:for-in
                  ;children --> (list var name object statement)
                  (let ((children (asn-children ast)))
                    (traverse (third children) append-doc-item)
                    (traverse (fourth children) append-doc-item)))

                 (:for
                  ;children --> (list var init test step statement)
                  ;init --> (nil | vardefs (see :var, above) | asn (an expression))
                  ;test, step --> (nil | asn (an expression)
                  (let* ((children (asn-children ast))
                         (init (second children))
                         (test (third children))
                         (step (fourth children))
                         (statement (fifth children)))
                    (if (asn-p init)
                        (traverse init append-doc-item)
                        (dolist (def init) ;def is a lexical-var
                          (if (lexical-var-init-val def)
                              (traverse (lexical-var-init-val def) append-doc-item))))
                    (and test (traverse test append-doc-item))
                    (and step (traverse step append-doc-item))
                    (and statement (traverse statement append-doc-item))))

                 ((:function-def :function-literal)
                  ;children --> (list name parameter-list body)
                  ;body --> is a list of statements
                  (setf (asn-doc ast) (create-function-doc ast #'ensure-doc-when-comment-exists))
                  (push (asn-doc ast) doc-stack)
                  (dolist (statement (third (asn-children ast)))
                    (traverse statement append-doc-item))

                  (let ((s (first (third (asn-children ast))))) ;s is the first statement in the function
                    (let ((s (and s (eq (asn-type s) :statement) (asn-children s)))) ;s is the expr of a simple statement or nil
                      (let ((fname (and s (eq (asn-type s) :call) (get-ast-name (car (asn-children s)))))) ;fname is the name of a called function or nil
                        (if (equal fname "bd.docGen")
                            (setf 
                             (doc-overloads (asn-doc ast)) (doc-overloads (asn-doc s))
                             (doc-kwargs (asn-doc ast)) (doc-kwargs (asn-doc s))))))))

                 (:if
                  ;children --> (list condition then else)
                  (let* ((children (asn-children ast))
                         (condition (first children))
                         (then (second children))
                         (else (third children)))
                    (traverse condition append-doc-item)
                    (traverse then append-doc-item)
                    (and else (traverse condition append-doc-item))))

                 (:try
                  ;children --> (list body catch finally)
                  (let* ((children (asn-children ast))
                         (body (first children))
                         (catch (second children))
                         (finally (third children)))
                    (traverse body append-doc-item)
                    (and catch (traverse (cdr catch) append-doc-item))
                    (and finally (traverse finally append-doc-item))))

                 (:expr-list
                  ;children --> list of expressions
                  (dolist (expr (asn-children ast))
                    (traverse expr append-doc-item)))

                 (:new
                  ;children --> (new-expr . args)
                  ;args an expr-list asn
                  (create-doc ast)
                  (let* ((children (asn-children ast)))
                    (traverse (car children) append-doc-item)
                    (traverse (cdr children) append-doc-item)))

                 ((:unary-prefix :unary-postfix)
                  ;children --> (cons (token-value op) expr)
                  (create-doc ast)
                  (traverse (cdr (asn-children ast)) append-doc-item))

                 (:array
                  ;children --> expr-list
                  (create-doc ast)
                  (dolist (item (asn-children ast))
                    (traverse item append-doc-item)))

                 (:object
                  ;children --> list of (property-name . expression)
                  (create-doc ast)
                  (let ((mark-doc (asn-doc ast))
                        (has-content nil))
                    (dolist (property (asn-children ast))
                      (let ((property-name (car property))
                            (property-value (cdr property)))
                        (when mark-doc
                          (ensure-doc-when-comment-exists (asn-comment property-name))
                          (ensure-doc-when-comment-exists (asn-comment property-value)))
                        (create-doc property-name)
                        (traverse (cdr property) append-doc-item)
                        (when (asn-doc property-name)
                          (setf has-content t (doc-location (asn-doc property-name)) (asn-location property-value)))
                        (when (asn-doc property-value)
                          (setf has-content t (doc-location (asn-doc property-value)) (asn-location property-value)))))
                    (if (and has-content (not (asn-doc ast)))
                        (setf (asn-doc ast) (make-doc :type :variable))))
                  (if (asn-doc ast)
                      (setf (doc-location (asn-doc ast)) (asn-location ast)
                            (doc-properties (asn-doc ast)) (asn-children ast))))

                 ((:atom :num :string :regexp :name)
                  (create-doc ast))

                 ((:= :+= :-= :/= :*= :%= :>>= :<<= :>>>= :~= :%= :|\|=| :^=)
                  ;children --> (lhs . rhs)
                  
                  (let* ((children (asn-children ast))
                         (lhs (car children))
                         (rhs (cdr children))
                         (name (get-ast-name lhs)))
                    (if (asn-comment ast)
                        (create-doc ast))
                    (traverse rhs append-doc-item)
                    (let ((doc (or (asn-doc ast) (asn-doc rhs))))
                      (if (and name doc)
                          (progn
                            (setf (doc-location doc) (asn-location ast))
                            (if (cl-ppcre:scan kwargs name)
                                (setf (doc-type doc) :type))
                            (funcall append-doc-item name doc))))))


                 (:call
                  ;children --> (function-expression . args)
                  ;args --> (expression list)
                  (create-doc ast)
                  (let* ((children (asn-children ast))
                         (func-expr (car children))
                         (args (cdr children))
                         (function-name (get-ast-name func-expr)))
                    (if (and (equal function-name "dojo.def") (eq (asn-type (first args)) :string))
                        (let ((module-name (token-value (asn-children (first args)))))
                         (labels ((append-doc-item (name doc)
                                    (setf (doc-defining-module doc) module-name)
                                    (funcall append-doc-item name doc)))
                           (traverse func-expr #'append-doc-item)
                           (dolist (arg args)
                             (traverse arg #'append-doc-item))))
                        (progn
                          (traverse func-expr append-doc-item)
                          (dolist (arg args)
                            (traverse arg append-doc-item))))
                    (cond
                      ((equal function-name "bd.declare")
                       (process-bd-declare ast args append-doc-item))
                      
                      ((or (equal function-name "bd.attr") (equal function-name "bd.domAttr"))
                       (push :attr (doc-flags (asn-doc ast))))
                             
                      ((or (equal function-name "bd.constAttr") (equal function-name "bd.constDomAttr"))
                       (push :roAttr (doc-flags (asn-doc ast))))
                       
                      ((or (equal function-name "dojo.mix") (equal function-name "bd.mix"))
                       (process-dojo-mixin args get-doc-item append-doc-item))

                      ((equal function-name "dojo.provide")
                       (process-dojo-provide args resource))

                      ((equal function-name "dojo.require")
                       (process-dojo-require args resource))

                      ((equal function-name "dojo.def")
                       (process-dojo-def args resource (asn-location ast) append-doc-item))

                      ((equal function-name "bd.docGen")
                       (push (asn-location ast) (doc-bd-doc-blocks (resource-doc resource)))
                       (process-bd-doc args append-doc-item ast)))))
                 

                 ((:dot :sub :comma :|\|\|| :&& :|\|| :^ :& :== :=== :!= :!== :< :> :<= :>= :instanceof :>> :<< :>>> :+ :- :* :/ :%)
                  (create-doc ast))
                 
                 (:conditional
                  ;children --> (list condition true-expr false-expr)
                  (create-doc ast)
                  (let* ((children (asn-children ast))
                         (condition (first children))
                         (true-expr (second children))
                         (false-expr (third children)))
                    (traverse condition append-doc-item)
                    (traverse true-expr append-doc-item)
                    (traverse false-expr append-doc-item)))
                 )))
      (traverse (resource-ast resource) append-doc-item))))
