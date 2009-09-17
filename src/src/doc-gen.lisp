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
      (let ((candidate (find-if (lambda (chunk) (eq (doc-chunk-pragma chunk) :md)) (doc-ldoc doc))))
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
  (if param-list
      (let ((params (make-doc-params)))
        (dolist (p param-list params)
          (vector-push-extend (get-doc-param (car p) (cdr p)) params)))))

(defun create-*-doc (ast type legal-pragmas)
  (let ((doc (make-doc :type type))
        (raw-doc (asn-comment ast)))
    (if raw-doc
        (do* ((text (sift-pragmas raw-doc legal-pragmas))
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

            (:return
              (setf text (get-doc-return/throw-subsection pragma text (doc-get-returns doc))))

            (:throw
                (setf text (get-doc-return/throw-subsection pragma text (doc-get-throws doc))))
            
            (:end
             (setf text (cdr text)))
            
            (t
             (setf text (get-doc-chunk (or pragma :md) text (doc-get-ldoc doc))))))
        (fixup-sdoc doc))
    doc))

(defun create-array-doc (ast)
  (create-*-doc ast :variable #'map-object-pragmas))

(defun create-object-doc (ast)
  (create-*-doc ast :variable #'map-object-pragmas))

(defun create-function-doc (ast)
  (let ((doc (create-*-doc ast :function #'map-function-pragmas)))
    ;second of ast.children is a list of (name . comment) pairs, one for each parameter
    (and doc (setf (doc-params doc) (get-doc-params (asn-children ast))))
    doc))

(defun process-resource-comment (
  raw-doc ;the raw documentation
  resource ;the resource that contained the raw-doc
)
  (declare (ignore raw-doc resource))

  ;;TODO
)

;;
;; These are the special processing functions
;;
(defun process-dojo-declare (arg-list)
  arg-list
)
#|
  (labels ((get-super-list (supers)
             (if (eq (asn-type supers) :array)
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
|#

(defun process-dojo-mixin (arg-list)
  arg-list
)
#|
  (when (eq (length (third ast)) 2)
    (let* ((name (get-ast-name (first (third ast))))
           (arg2 (second (third ast)))
           (members (and (eq (asn-type arg2) :object) (create-object-doc arg2 "force-doc" nil))))
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
(defun get-ast-name (ast)
  (case (asn-type ast)
    (:name
     (token-value (asn-children ast)))
    (:dot
     (concatenate 'string 
                  (get-ast-name (car (asn-children ast))) 
                  "." 
                  (token-value (cdr (asn-children ast)))))
    (t nil)))

(defun traverse (resource)
  (let ((doc-stack nil))
    (labels ((traverse (ast)
               (case (asn-type ast)
                 ((:root :block) 
                  ;children --> list of statements
                  (dolist (statement (asn-children ast))
                    (traverse statement)))

                 (:comment
                  ;children --> comment-token
                  (process-resource-comment (asn-children ast) resource))

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
                       ;todo---push the comment into the current function
                       nil)
                   (traverse (asn-children ast)))

                 (:throw
                  ;children --> throw-expr
                  (if (asn-comment ast)
                      ;todo---push the comment into the current function
                      nil)
                  (traverse (asn-children ast)))

                 (:var
                  ;children --> vardefs
                  ;vardefs is a list of (name . expr)
                  ;name is a token, expr is an expression or nil
                  (dolist (def (asn-children ast))
                    (if (cdr def)
                        (traverse (cdr def)))))

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
                        (dolist (def init)
                          (if (cdr def)
                              (traverse (cdr def)))))
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
                  ;args a list of expressions
                  (setf (asn-doc ast) (create-*-doc ast :variable #'map-object-pragmas))
                  (let* ((children (asn-children ast))
                         (new-expr (car children))
                         (args (cdr children)))
                    (traverse new-expr)
                    (dolist (expr args)
                      (traverse expr))))

                 ((:unary-prefix :unary-postfix)
                  ;children --> (cons (token-value op) expr)
                  (setf (asn-doc ast) (create-*-doc ast :variable #'map-object-pragmas))
                  (traverse (cdr (asn-children ast))))

                 (:array
                  ;children --> expr-list
                  (setf (asn-doc ast) (create-array-doc ast))
                  (dolist (item (asn-children ast))
                    (traverse item)))

                 (:object
                  ;children --> list of (property-name . expression)
                  (setf (asn-doc ast) (create-object-doc ast))
                  (let ((members (make-hash-table :test 'equal)))
                    (dolist (property (asn-children ast))
                      (let ((name (car property))
                            (value (cdr property)))
                        (traverse (cdr property))
                        (setf (gethash name members) (asn-doc value))))
                    (setf (doc-members (asn-doc ast)) members)))

                 ((:atom :num :string :regexp :name)
                  (setf (asn-doc ast) (create-*-doc ast :variable #'map-object-pragmas)))

                 ((:+= :-= :/= :*= :%= :>>= :<<= :>>>= :~= :%= :|\|=| :^=)
                  ;children --> (cons lhs rhs)
                  (let* ((children (asn-children ast))
                         (lhs (car children))
                         (rhs (cdr children))
                         (name (get-ast-name lhs)))
                    (if (asn-comment ast)
                        (progn 
                          (if (asn-comment rhs) 
                              (format t "WARNING: two attempts to document the same item.~%"))
                          (setf (asn-comment rhs) (asn-comment ast))))
                    (traverse rhs)
                    (let ((doc (asn-doc rhs)))
                      (if (and name doc)
                          (progn
                            (setf (doc-location doc) (asn-location ast))
                            (append-doc name doc))))))

                 (:call
                  ;children --> (function-expression . args)
                  ;args --> (expression list)
                  (setf (asn-doc ast) (create-*-doc ast :variable #'map-object-pragmas))
                  (let* ((children (asn-children ast))
                         (lhs (car children))
                         (rhs (cdr children))
                         (function-name (get-ast-name lhs)))
                    (cond
                      ((equal function-name "dojo.declare")
                       (process-dojo-declare ast))

                      ((equal function-name "dojo.mixin")
                       (process-dojo-mixin rhs))

                      (t
                       (traverse lhs)
                       (dolist (arg rhs)
                         (traverse arg))))))
                 
                 ((:dot :sub :comma :|\|\|| :&& :|\|| :^ :& :== :=== :!= :!== :< :> :<= :>= :instanceof :>> :<< :>>> :+ :- :* :/ :%)
                  (setf (asn-doc ast) (create-*-doc ast :variable #'map-object-pragmas)))
                 
                 (:conditional
                  ;children --> (list condition true-expr false-expr)
                  (setf (asn-doc ast) (create-*-doc ast :variable #'map-object-pragmas))
                  (let* ((children (asn-children ast))
                         (condition (first children))
                         (true-expr (second children))
                         (false-expr (third children)))
                    (traverse condition)
                    (traverse true-expr)
                    (traverse false-expr)))
                 )))
      (traverse (resource-ctrl-ast resource)))))

(defun generate-docs (resource)
  ;;keep the source out of the object for debugging...
  (setf *current-source* nil)

  (clrhash *doc-items*)
  (traverse resource)
  (dump-doc-items *standard-output*)
)
