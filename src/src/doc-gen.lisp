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
    (dolist (word '(:mu :md :namespace :const :enum :type :variable :kwargs :hash :nosource :note :warn :code :return :throw :case :result :todo :todoc :inote :file))
      (setf (gethash (string-downcase (string word)) pragmas) word))
    (setf 
     (gethash "/" pragmas) :end
     (gethash "*" pragmas) :endParamType

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

(defun map-object-pragmas (pragma args)
  (if (and (not pragma) args)
      :paramType
      (let ((pragma (gethash pragma *pragma-map*)))
        (case pragma
          ((:mu :md :note :warn :code :todo :todoc :inote :end :endParamType :namespace :const :enum :type :variable :kwargs :hash :nosource) pragma)
          (t nil)))))

(defun map-function-pragmas (pragma args)
  (declare (ignore args))
  (let ((pragma (gethash pragma *pragma-map*)))
    (case pragma
      ((:mu :md :note :warn :code :todo :todoc :inote :end :return :throw :case :result :nosource) pragma)
      (t nil))))

(defun map-parameter-pragmas (pragma args)
  (if (or (eq pragma :result) (and (not pragma) args))
      :paramType
      (let ((pragma (gethash pragma *pragma-map*)))
        (case pragma
          ((:mu :md :case :note :warn :code :todo :todoc :inote :end :endParamType) pragma)
          (t nil)))))

(defun map-comment-pragmas (pragma args)
  (declare (ignore args))
  (let ((pragma (gethash pragma *pragma-map*)))
    (case pragma
      ((:file :mu :md :note :warn :code :todo :todoc :inote :end) pragma)
      (t nil))))
  

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

(defun get-doc-param-type-section (text)
  ;text is a sifted list of pragmas; the first pragma should be a :paramType
  ;process until another :paramType, :endParamType, or end is encountered
  (let ((type (line-pragma-args text))
        (section (make-doc-section)))
    (do* ((p text)
          (pragma :md (line-pragma p)))
         ((or (not p) (eq pragma :paramType) (eq pragma :endParamType))
          (values type section p))
      (case pragma
        ((:note :warn :code)
         (setf p (get-doc-simple-subsection pragma p section)))
        (:end
         (setf p (cdr p)))
        (t
         (setf p (get-doc-chunk (or pragma :md) p section)))))))

(defun get-doc-param (name raw-doc)
  (let ((param (make-doc-param name))
        (general-section (make-doc-section)))
    (do* ((text (sift-pragmas raw-doc #'map-parameter-pragmas))
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

        ((:end :endParamType)
         (setf text (cdr text)))
                 
        (t
         (setf text (get-doc-chunk (or pragma :md) text general-section)))))))

(defun get-doc-params (param-list)
  ;param-list is a list of (name . comment) pairs
  (if param-list
      (let ((params (make-doc-params)))
        (dolist (p param-list params)
          (vector-push-extend (get-doc-param (car p) (cdr p)) params)))))

(defun create-*-doc (raw-doc type legal-pragmas &optional (force nil))
  (if (or raw-doc force)
      (let ((doc (make-doc :type type)))
        (do* ((text (sift-pragmas raw-doc legal-pragmas))
              (pragma (line-pragma text) (line-pragma text)))
             ((not text))
          (case pragma
            ((:namespace :type :const :enum :variable) 
             (setf 
              (doc-type doc) pragma 
              text (cdr text)))
            
            ((:kwargs :hash :nosource)
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
              (setf text (get-doc-return/throw-subsection pragma text (doc-get-returns doc))))
            
            (:throw
                (setf text (get-doc-return/throw-subsection pragma text (doc-get-throws doc))))
            
            ((:end :endParamType)
             (setf text (cdr text)))
            
            (t
             (setf text (get-doc-chunk (or pragma :md) text (doc-get-ldoc doc))))))
        (fixup-sdoc doc))))

(defun create-doc (ast &optional (force nil))
  (setf (asn-doc ast) (create-*-doc (asn-comment ast) :variable #'map-object-pragmas force)))

(defun create-function-doc (ast)
  ;always create function docs because parameters may be defined or the body may contain return/throws
  ;in spite of the fact that no function definition exists
  (let ((doc (create-*-doc (asn-comment ast) :function #'map-function-pragmas t)))
    (setf 
     (doc-params doc) (get-doc-params (second (asn-children ast)))
     (asn-doc ast) doc)))

(defun process-return (ast current-doc)
  (do ((text (sift-pragmas (asn-comment ast) #'map-parameter-pragmas)))
      ((not text))
    (multiple-value-bind 
      (type section next) (get-doc-param-type-section text)
      (doc-push-return current-doc type section)
      (setf text next))))

(defun process-comment-island (
  raw-doc ;the raw documentation
  resource ;the resource that contained the raw-doc
  current-doc-item ;the current documentable item being processed
)
  (let ((text (sift-pragmas (token-value raw-doc) #'map-comment-pragmas))
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
        
        ((:todo :todoc :inote)
         (setf text (get-doc-simple-subsection pragma text (doc-get-inotes doc))))
        
        (:end
         (setf text (cdr text)))
        
        (t
         (setf text (get-doc-chunk (or pragma :md) text (doc-get-ldoc doc))))))
    (fixup-sdoc doc)))

;;
;; These are the special processing functions
;;

(defun process-dojo-declare (arg-list)
  (let ((class-name (token-value (asn-children (first arg-list))))
        (supers (second arg-list))
        (doc (asn-doc (third arg-list))))
    (setf 
     (doc-type doc) :class
     (doc-members doc) (doc-properties doc)
     (doc-properties doc) nil)
    (if (eq (asn-type supers) :array)
        (setf (doc-supers doc) (mapcar #'get-ast-name (asn-children supers))))
    (values class-name doc)))

(defun process-dojo-mixin (arg-list get-doc-item)
  (let* ((name (get-ast-name (first arg-list)))
         (src (and (eq (asn-type (second arg-list)) :object) (doc-properties (asn-doc (second arg-list)))))
         (dest (and src (or 
                (funcall get-doc-item name :namespace)
                (funcall get-doc-item name :variable)
                (funcall get-doc-item name :function)))))
    (if dest
        (maphash (lambda (name value)  (doc-push-property dest name value)) src))))

(defun process-dojo-provide (args resource)
  ;args is the list of argument expressions sent to dojo.provide
  ;we only pay attention if it a :string asn
  (if (eq (asn-type (first args)) :string)
      (let ((doc (resource-doc resource)))
        (push (token-value (asn-children (first args))) (doc-provides doc)))))

(defun process-dojo-require (args resource)
  ;args is the list of argument expressions sent to dojo.require
  ;we only pay attention if it a :string asn
  (if (eq (asn-type (first args)) :string)
      (let ((doc (resource-doc resource)))
        (push (token-value (asn-children (first args))) (doc-requires doc)))))

(defun process-bd-typedef (args append-doc-item)
  ;args is the list of argument expressions sent to bd.typedef; 
  ;first of args is an object that gives a hash of types
  (dolist (item (asn-children (first args)))
    ;item is a cons (name . value), both name and value or ast's, name should be a string
    (let* ((name (car item))
           (value (cdr item))
           (doc (or (asn-doc name) (asn-doc value))))
      ;(format t "~A~%" value)
      (setf (doc-type doc) :type)
      (funcall append-doc-item (token-value (asn-children name)) doc))))

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

(defun doc-gen (resource append-doc-item get-doc-item)
  (setf (resource-doc resource) (funcall get-doc-item (resource-name resource) :resource t))
  (let ((doc-stack (list (resource-doc resource))))
    (labels ((traverse (ast)
               (if (not ast)
                   (return-from traverse))
               (case (asn-type ast)
                 ((:root :block) 
                  ;children --> list of statements
                  (dolist (statement (asn-children ast))
                    (traverse statement)))

                 (:comment
                  ;children --> comment-token
                  (process-comment-island (asn-children ast) resource (first doc-stack)))

                 (:label
                  ;children --> (label . statement)
                  (traverse (second (asn-children ast))))
                 
                 (:switch
                  ;children --> (switch-expr  . case-list)
                  (let ((children (asn-children ast)))
                    (traverse (car children))
                    (dolist (case-item (cdr children))
                      (traverse case-item))))

                 (:case
                  ;children --> expression
                  (traverse (asn-children ast)))

                 (:default
                  ;children --> nil
                  )

                 (:debugger
                  ;children --> nil
                  )

                 (:do
                  ;children --> (condition . statment)
                  (let ((children (asn-children ast)))
                    (traverse (car children))
                    (traverse (cdr children))))

                 (:return
                  ;children --> expression
                   (if (asn-comment ast)
                       (process-return ast (first doc-stack)))
                   (traverse (asn-children ast)))

                 (:throw
                  ;children --> throw-expr
                  (if (asn-comment ast)
                      ;todo---push the comment into the current function
                      nil)
                  (traverse (asn-children ast)))

                 (:var
                  ;children --> vardefs
                  ;vardefs is a list of lexical-var
                  ;name is a token, expr is an expression or nil
                  (dolist (def (asn-children ast))
                    (and (lexical-var-init-val def) (traverse (lexical-var-init-val def)))))

                 (:while
                  ;children --> (while-condition . while-statement)
                  (let ((children (asn-children ast)))
                    (traverse (car children))
                    (traverse (cdr children))))

                 (:with
                  ;children --> (with-expr . with-statement)
                  (let ((children (asn-children ast)))
                    (traverse (car children))
                    (traverse (cdr children))))

                 (:statement
                  ;children --> expression
                  (traverse (asn-children ast)))

                 ((:break :continue)
                  ;children --> name-token
                  )

                 (:for-in
                  ;children --> (list var name object statement)
                  (let ((children (asn-children ast)))
                    (traverse (third children))
                    (traverse (fourth children))))

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
                        (traverse init)
                        (dolist (def init) ;def is a lexical-var
                          (if (lexical-var-init-val def)
                              (traverse (lexical-var-init-val def)))))
                    (and test (traverse test))
                    (and step (traverse step))
                    (and statement (traverse statement))))

                 ((:function-def :function-literal)
                  ;children --> (list name parameter-list body)
                  ;body --> is a list of statements
                  (setf (asn-doc ast) (create-function-doc ast))
                  (push (asn-doc ast) doc-stack)
                  (dolist (statement (third (asn-children ast)))
                    (traverse statement))
                  (pop doc-stack))

                 (:if
                  ;children --> (list condition then else)
                  (let* ((children (asn-children ast))
                         (condition (first children))
                         (then (second children))
                         (else (third children)))
                    (traverse condition)
                    (traverse then)
                    (and else (traverse condition))))

                 (:try
                  ;children --> (list body catch finally)
                  (let* ((children (asn-children ast))
                         (body (first children))
                         (catch (second children))
                         (finally (third children)))
                    (dolist (statement body)
                      (traverse statement))
                    (and catch (traverse catch))
                    (and finally (traverse finally))))

                 (:expr-list
                  ;children --> list of expressions
                  (dolist (expr (asn-children ast))
                    (traverse expr)))

                 (:new
                  ;children --> (new-expr . args)
                  ;args an expr-list asn
                  (create-doc ast)
                  (let* ((children (asn-children ast)))
                    (traverse (car children))
                    (traverse (cdr children))))

                 ((:unary-prefix :unary-postfix)
                  ;children --> (cons (token-value op) expr)
                  (create-doc ast)
                  (traverse (cdr (asn-children ast))))

                 (:array
                  ;children --> expr-list
                  (create-doc ast)
                  (dolist (item (asn-children ast))
                    (traverse item)))

                 (:object
                  ;children --> list of (property-name . expression)
                  ;always create a doc since this may be an object literal used in dojo.mixin or dojo.declare
                  (dolist (property (asn-children ast))
                    (create-doc (car property))
                    (traverse (cdr property)))
                  (create-doc ast t)
                  (setf (doc-properties (asn-doc ast)) (asn-children ast))
;                  (format t "~A~%" ast)
)

                 ((:atom :num :string :regexp :name)
                  (create-doc ast))

                 ((:= :+= :-= :/= :*= :%= :>>= :<<= :>>>= :~= :%= :|\|=| :^=)
                  ;children --> (lhs . rhs)
                  
                  (let* ((children (asn-children ast))
                         (lhs (car children))
                         (rhs (cdr children))
                         (name (get-ast-name lhs)))
                    (if (asn-comment ast)
                      ;the comment is really documenting the lhs
                      (setf (asn-comment lhs) (asn-comment ast)
                            (asn-comment ast) nil))
                    (traverse rhs)
                    (let ((doc (asn-doc rhs)))
                      (if (and name doc)
                          (progn
                            (setf (doc-location doc) (asn-location ast))
                            (funcall append-doc-item name doc))))))

                 (:call
                  ;children --> (function-expression . args)
                  ;args --> (expression list)
                  (create-doc ast) ;TODO this is a little weird; it's an expression
                  (let* ((children (asn-children ast))
                         (func-expr (car children))
                         (args (cdr children))
                         (function-name (get-ast-name func-expr)))
                    (traverse func-expr)
                    (dolist (arg args)
                      (traverse arg))
                    (cond
                      ((equal function-name "dojo.declare")
                       (multiple-value-bind (class-name doc) (process-dojo-declare args)
                         (funcall append-doc-item class-name doc)))
                       
                      ((equal function-name "dojo.mixin")
                       (process-dojo-mixin args get-doc-item))

                      ((equal function-name "dojo.provide")
                       (process-dojo-provide args resource))

                      ((equal function-name "dojo.require")
                       (process-dojo-require args resource))

                      ((equal function-name "bd.typedef")
                       (process-bd-typedef args append-doc-item))

                      )))
                 
                 ((:dot :sub :comma :|\|\|| :&& :|\|| :^ :& :== :=== :!= :!== :< :> :<= :>= :instanceof :>> :<< :>>> :+ :- :* :/ :%)
                  (create-doc ast))
                 
                 (:conditional
                  ;children --> (list condition true-expr false-expr)
                  (create-doc ast)
                  (let* ((children (asn-children ast))
                         (condition (first children))
                         (true-expr (second children))
                         (false-expr (third children)))
                    (traverse condition)
                    (traverse true-expr)
                    (traverse false-expr)))
                 )))
      (traverse (resource-ast resource)))))
