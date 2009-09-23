(in-package #:js-proc)

(defparameter *unary-prefix* 
  '(:typeof :void :delete :-- :++ :! :~ :- :+))

(defun unary-prefix-p (token)
  (and (token-type-p token :operator) (member (token-value token) *unary-prefix*)))

(defparameter *unary-postfix*
  '(:-- :++))

(defun unary-postfix-p (token)
  (and (token-type-p token :operator) (member (token-value token) *unary-postfix*)))

(defparameter *assignment*
  (let ((assign (make-hash-table)))
    (dolist (op '(:= :+= :-= :/= :*= :%= :>>= :<<= :>>>= :~= :%= :|\|=| :^=))
      (setf (gethash op assign) t))
    assign))

(defun assignment-p (token)
  (and (token-type-p token :operator) (gethash (token-value token) *assignment*)))

(defparameter *precedence*
  (let ((precs (make-hash-table)))
    (loop :for ops :in '((:|\|\||) (:&&) (:|\||) (:^) (:&) (:== :=== :!= :!==)
                         (:< :> :<= :>= :in :instanceof) (:>> :<< :>>>) (:+ :-) (:* :/ :%))
          :for n :from 1
          :do (dolist (op ops) (setf (gethash op precs) n)))
    precs))

(defstruct asn
  type     ;(symbol) the type of the production
  comment  ;(vector of string) the comment associated with the production
  doc      ;(doc) the documentation associated with the production, if any
  location ;(location) the location in the source for this production
  children ;(token | pair | list)
)

(defstruct lexical-var
  name
  init-val
  comment
)

(defparameter strict-semicolons t)
(defparameter *in-function* nil)
(defparameter *in-loop* nil)
(defvar tokens)
(defvar token-index)
(defvar peeked)
(defvar token)
(defvar label-stack)

(defun advance ()
  (prog1 (aref tokens token-index) (incf token-index)))

(defun get-next-token (&optional (comment-ok nil))
  (setf comment-ok t)
  (do ((token (advance) (advance)))
      ((or (not (token-type-p token :comment)) comment-ok)
       token)))

(defun peek ()
  (or peeked (setf peeked (get-next-token))))

(defun next (&optional (comment-ok nil))
  (if peeked
      (setf token peeked peeked nil)
      (setf token (get-next-token comment-ok)))
  token)

(defun skip (n)
  (dotimes (i n token) (next)))

(defun get-token-and-advance ()
  (prog1 token (next)))

(defun token-error (token control &rest args)
  (let ((*line* (token-line token)) (*char* (token-char token)))
    (apply #'js-parse-error control args)))

(defun error* (control &rest args)
  (apply #'token-error token control args))

(defun unexpected (token)
  (token-error token "Unexpected token '~a'." (token-id token)))

(defun expect-token (type val &optional (comment-ok nil))
  (if (tokenp token type val)
      (prog1 token (next comment-ok))
      (error* "Unexpected token '~a', expected '~a'." (token-id token) val)))

(defun expect (punc &optional (comment-ok nil))
  (expect-token :punc punc comment-ok))

(defun expect-key (keyword)
  (expect-token :keyword keyword))

(defun semicolon ()
  (cond (strict-semicolons (expect #\; t))
        ((tokenp token :punc #\;) (prog1 token (next t)))
        ((token-newline-before token) nil)
        (t (unexpected token))))

(defun get-location (src)
  (typecase src
    (asn (asn-location src))
    (token (token-location src))
    (t src)))

(defun sum-locations (start-location end-location)
  (let ((start (get-location start-location))
        (end (get-location end-location)))
    (make-location :start-line (location-start-line start)
                   :start-char (location-start-char start)
                   :end-line (location-end-line end)
                   :end-char (location-end-char end))))

(defun as-comment (comment-token)
  (make-asn 
   :type :comment
   :location (token-location comment-token)
   :children comment-token))

(defun as-label (label statement)
  (make-asn 
   :type :label
   :location (sum-locations label statement)
   :children (cons label statement)))

(defun as-block (opening-brace statements closing-brace)
  (make-asn
   :type :block
   :location (sum-locations opening-brace closing-brace)
   :children statements))

(defun as-switch (switch-token switch-expr case-list right-brace-token)
  ;;note, as it stands, the case-list can contain any kind of statements; it should
  ;;only contain case and default statements
  (make-asn
   :type :switch
   :location (sum-locations switch-token right-brace-token)
   :children (cons switch-expr case-list)))

(defun as-case (case-token expression colon-token)
  (make-asn
   :type :case
   :location (sum-locations  case-token colon-token)
   :children expression))

(defun as-default (default-token colon-token)
  (make-asn
   :type :default
   :location (sum-locations default-token colon-token)))

(defun as-debugger (debugger-token semicolon-token)
  (make-asn
   :type :debugger
   :location (sum-locations debugger-token (or semicolon-token debugger-token))))

(defun as-do (do-token body condition semicolon-token)
  (make-asn
   :type :do
   :location (sum-locations do-token (or semicolon-token condition))
   :children (cons condition body)))

(defun as-return (return-token expression semicolon-token)
  (make-asn
   :type :return
   :comment (and semicolon-token (token-comment semicolon-token))
   :location (sum-locations return-token (or semicolon-token expression return-token))
   :children expression))

(defun as-throw (throw-token throw-expr semicolon-token)
  (make-asn
   :type :throw
   :comment (and semicolon-token (token-comment semicolon-token))
   :location (sum-locations throw-token (or semicolon-token throw-expr))
   :children throw-expr))

(defun as-var (var-token vardefs semicolon-token)
  ;;vardefs is a list of (name . expr); name is a token, expr is an expression or nil
  (make-asn
   :type :var
   :location (sum-locations var-token (or semicolon-token (last vardefs)))
   :children vardefs))

(defun as-while (while-token while-condition while-statement)
  (make-asn
   :type :while
   :location (sum-locations while-token (or while-statement while-condition))
   :children (cons while-condition while-statement)))

(defun as-with (with-token with-expr with-statement)
  (make-asn
   :type :with
   :location (sum-locations with-token (or with-statement with-expr))
   :children (cons with-expr with-statement)))

(defun as-statement (expression semicolon-token)
  (make-asn
   :type :statement
   :comment (and semicolon-token (token-comment semicolon-token))
   :location (sum-locations expression (or semicolon-token expression))
   :children expression))

(defun as-break/cont (break/cont-token name-token semicolon-token)
  (make-asn
   :type (token-value break/cont-token)
   :location (sum-locations break/cont-token (or semicolon-token name-token break/cont-token))
   :children name-token))

(defun as-for-in (for-token var name object statement close-paren-token)
  (make-asn
   :type :for-in
   :location (sum-locations for-token (or statement close-paren-token))
   :children (list var name object statement)))

(defun as-for (for-token var init test step statement close-paren-token)
  (make-asn
   :type :for
   :location (sum-locations for-token (or statement close-paren-token))
   :children (list var init test step statement)))

(defun as-function (function-token statement name parameter-list comment body right-brace)
  (make-asn
   :type (if statement :function-def :function-literal)
   :location (sum-locations function-token right-brace)
   :comment comment
   :children (list name parameter-list body)))

(defun as-if (if-token condition body else)
  (make-asn
   :type :if
   :location (sum-locations if-token (or else body))
   :children (list condition body else)))

(defun as-try (try-token body catch finally)
  (make-asn
   :type :try
   :location (sum-locations try-token (or finally catch body))
   :children (list body catch finally)))

(defun as-expr-list (opening-token expr-list closing-token)
  (make-asn
   :type :expr-list
   :location (sum-locations opening-token closing-token)
   :children expr-list))

(defun as-new (new-token new-expr args)
  (make-asn
   :type :new
   :location (sum-locations new-token args)
   :children (cons new-expr args)))

(defun as-unary-prefix (op expr)
  ;(when (and (member op '(:++ :-- :delete)) (not (is-assignable expr)))
  ;  (error* "Invalid use of '~a' operator." op))
  (make-asn
   :type :unary-prefix
   :location (sum-locations op expr)
   :children (cons (token-value op) expr)))

(defun as-unary-postfix (op expr)
  ;(when (and (member op '(:++ :-- :delete)) (not (is-assignable expr)))
  ;  (error* "Invalid use of '~a' operator." op))
  (make-asn
   :type :unary-postfix
   :location (sum-locations expr op)
   :children (cons (token-value op) expr)))

(defun as-array (opening-bracket expr-list closing-bracket)
  (make-asn
   :type :array
   :comment (token-comment opening-bracket)
   :location (sum-locations opening-bracket closing-bracket)
   :children expr-list))

(defun as-object (opening-brace property-list closing-brace)
  (make-asn
   :type :object
   :comment (token-comment opening-brace)
   :location (sum-locations opening-brace closing-brace)
   :children property-list))

(defun as-atom (token)
  (make-asn
   :type (token-type token) ;one of :atom :num :string :regexp :name
   :location (sum-locations token token)
   :children token))

(defun as-binary-op (lhs op rhs &optional location)
  (make-asn
   :type (or (and (symbolp op) op) (token-value op)) ;one of... 
     ; :dot :sub :call :comma
     ; :\\ :&& :\ :^ :& :== :=== :!= :!== :< :> :<= :>= :in :instanceof :>> :<< :>>> :+ :- :* :/ :%
     ; :+= :-= :/= :*= :%= :>>= :<<= :>>>= :~= :%= :\= :^=
   :comment (and (token-p op) (token-comment op))
   :location (or location (sum-locations lhs rhs))
   :children (cons lhs rhs)))

(defun as-conditional (condition true-expr false-expr)
  (make-asn
   :type :conditional
   :location (sum-locations condition false-expr)
   :children (list condition true-expr false-expr)))

(defun as-root (statements)
  (make-asn
   :type :root
   :location (sum-locations (car statements) (car (last statements)))
   :children statements))

(defun maybe-before-semicolon (func)
  (if (tokenp token :punc #\;)
      (prog1 (values nil nil) (next))
      (let ((start token))
        (handler-case (values (funcall func) (semicolon))
          (js-parse-error (err)
            (unless (and (eq token start) (token-newline-before token) (not strict-semicolons)) (error err))
            (values nil nil))))))

(defun vardefs ()
  (unless (token-type-p token :name) (unexpected token))
  (let ((name (get-token-and-advance)) 
        init-val
        comment)
    (when (tokenp token :operator :=)
      (setf comment (token-comment (get-token-and-advance))
            init-val (expression)))
    (if (tokenp token :punc #\,)
        (progn 
          (setf comment (or (token-comment (get-token-and-advance)) comment))
          (cons (make-lexical-var :name name :init-val init-val :comment comment) (vardefs)))
        (list (make-lexical-var :name name :init-val init-val :comment (or (and (tokenp token :punc #\;) (token-comment token)) comment))))))

(defun statement (&optional allow-case)
  (case (token-type token)
    ((:comment :block-comment)
     (prog1 (as-comment token) (next)))

    ((:num :string :regexp :operator :atom) 
     (simple-statement))

    (:name 
     (if (tokenp (peek) :punc #\:)
         (labeled-statement (prog1 token (skip 2)))
         (simple-statement)))

    (:punc 
     (case (token-value token)
       (#\{
        (block*))
       
       ((#\[ #\()
        (simple-statement))

       (#\;
        (prog1 (as-block token nil token) (next)))

       (t (unexpected token))))

    (:keyword
     (case (prog1 (token-value token) (next))
       ((:break :continue)
        (break/cont token))
       
       (:case
        (if allow-case
            (as-case token (expression) (expect #\:))
            (unexpected token)))

       (:default
        (if allow-case
            (as-default token (expect #\:))
            (unexpected token)))

       (:debugger 
        (as-debugger token (semicolon)))


       (:do 
        (let ((do-token token)
              (body (let ((*in-loop* t)) (statement)))
              (condition (and (expect-key :while) (parenthesised))))
          (as-do do-token condition body (semicolon))))

       (:for 
        (for* token))

       (:function 
        (function* token t))

       (:if 
        (if* token))

       (:return
        (if *in-function* 
            (multiple-value-bind (expr semicolon-token) (maybe-before-semicolon #'expression)
              (as-return token expr semicolon-token))
            (error* "'return' outside of function.")))

       (:switch 
        (let ((switch-token token)
              (switch-expr (parenthesised))
              (case-list (and (expect #\{) (let ((*in-loop* t))
                                             (loop :until (tokenp token :punc #\})
                                                :collect (statement t))))))
          (as-switch switch-token switch-expr case-list (expect #\}))))

       (:throw 
        (as-throw token (expression) (semicolon)))    

       (:try 
        (try* token))

       (:var
        (as-var token (vardefs) (semicolon)))

       (:while 
        (as-while token (parenthesised) (let ((*in-loop* t)) (statement))))

       (:with
        (as-with token (parenthesised) (statement)))

       (t (unexpected token))))
    (t (unexpected token))))

(defun labeled-statement (label-token)
  (push (token-value label-token) label-stack)
  (let ((statement (statement)))
    (pop label-stack)
    (as-label label-token statement)))

;;
;;the original code said (unless (member (car stat) '(:for :do :while :switch)) (unexpected start))
;;but this is not a requirement
;;

(defun simple-statement ()
  (as-statement (expression) (semicolon)))

(defun break/cont (break/cont-token)
  (unless *in-loop* (error* "'~a' not inside a loop or switch." (token-value break/cont-token)))
  (let ((name (and (token-type-p token :name) token)))
    (when name (next))
    (unless (or (not name) (member (token-value name) label-stack :test #'string=))
      (error* "Labeled '~a' without matching loop or switch statement." (token-value break/cont-token)))
    (as-break/cont break/cont-token name (semicolon))))

(defun block* ()
  ;on entry, token must be "{"
  (let ((start-token token)
        (statements (progn (next)
                           (loop 
                              :until (tokenp token :punc #\}) 
                              :collect (statement)))))
    (prog1 (as-block start-token statements token) (next))))

(defun for* (for-token)
  (expect #\()
  (let ((var (and (tokenp token :keyword :var) token)))
    (when var (next))
    (if (and (token-type-p token :name) (tokenp (peek) :operator :in))
        (let ((name (token-value token))
              (obj (and (skip 2) (expression)))
              (close-paren (expect #\))))
          (as-for-in for-token var name obj (let ((*in-loop* t)) (statement)) close-paren))
        (let ((init (maybe-before-semicolon (if var #'vardefs #'expression)))
              (test (maybe-before-semicolon #'expression))
              (step (if (tokenp token :punc #\)) nil (expression)))
              (close-paren (expect #\))))
          (as-for for-token var init test step (let ((*in-loop* t)) (statement)) close-paren)))))

(defun lambda-list ()
  (cond
    ((tokenp token :punc #\))
     (next)
     nil)

    ((token-type-p token :name)
     (let ((name (token-value token))
           (comment (token-comment token)))
       (next)
       (if (tokenp token :punc #\,)
           (progn 
             (setf comment (token-comment token))
             (next)))
       (cons (cons name comment) (lambda-list))))

    (t (unexpected token))))

(defun function* (function-token statement)
  ;statement implies a definition like function x(...){...}
  ;not statement implies a function literal expression e.g....
  ;  x= function(...){...} 
  ;  someFunction(x, function(...){...}, ...)
  (let (name parameter-list comment body)
    (if (token-type-p token :name)
        (setf name (get-token-and-advance))) 
    (if (and statement (not name)) 
        (unexpected token))
    (expect #\()
    (setf parameter-list (lambda-list))
    (setf comment (token-comment (expect #\{)))
    (setf body (let ((*in-function* t) (*in-loop* nil))
                 (loop :until (tokenp token :punc #\})
                       :collect (statement))))
    (as-function function-token statement name parameter-list comment body (expect #\}))))

(defun if* (if-token)
  (let ((condition (parenthesised))
        (body (statement))
        (else (and (tokenp token :keyword :else) (next) (statement))))
    (as-if if-token condition body else)))
    
(defun try* (try-token)
  (let ((body (statement)) catch finally)
    (when (tokenp token :keyword :catch)
      (next) (expect #\()
      (unless (token-type-p token :name) (error* "Name expected."))
      (let ((name (token-value token)))
        (next) (expect #\))
        (setf catch (cons name (statement)))))
    (when (tokenp token :keyword :finally)
      (next)
      (setf finally (statement)))
    (as-try try-token body catch finally)))

;;
;; expression evaluator begins here...
;;
(defun expr-list (closing)
  (loop 
     :for first := t :then nil
     :until (tokenp token :punc closing)
     :unless first :do (expect #\,)
     :collect (expression nil)))

(defun parenthesised ()
  (let ((opening (expect #\())
        (expr (expression))
        (closing (expect #\))))
    (setf (asn-location expr) (sum-locations opening closing))
    expr))

(defun array* ()
  (let ((opening (expect #\[))
        (expr-list (expr-list #\]))
        (closing (expect #\])))
    (as-array opening expr-list closing)))

(defun object* ()
  (let ((opening (expect #\{))
        (property-list (loop 
                          :for first := t :then nil
                          :until (tokenp token :punc #\})
                          :unless first :do (expect #\,)
                          :collect (let* ((name (as-property-name))
                                          (colon-token (expect #\:))
                                          (rhs (expression nil)))
                                     (if (not (asn-comment rhs))
                                         (setf (asn-comment rhs) (token-comment colon-token)))
                                     (cons name rhs))))
        (closing (expect #\})))
    (as-object opening property-list closing)))

(defun new* (new-token)
  (next)
  (let ((new-expr (expr-atom nil))
        (args (if (tokenp token :punc #\()
                  (let ((opening (get-token-and-advance))
                        (args (expr-list #\)))
                        (closing (get-token-and-advance)))
                    (as-expr-list opening args closing)))))
    (subscripts (as-new new-token new-expr args) t)))

(defun expr-atom (allow-calls)
  (cond 
    ((tokenp token :operator :new)
     (new* token))

    ((unary-prefix-p token)
     (as-unary-prefix (get-token-and-advance) (expr-atom allow-calls)))
        
    ((token-type-p token :punc)
     (case (token-value token)
       (#\( (subscripts (parenthesised) allow-calls))
       (#\[ (subscripts (array*) allow-calls))
       (#\{ (subscripts (object*) allow-calls))
       (t (unexpected token))))
        
    ((tokenp token :keyword :function)
     (function* (get-token-and-advance) nil))
        
    ((member (token-type token) '(:atom :num :string :regexp :name))
     (subscripts (as-atom (get-token-and-advance)) allow-calls))
        
    (t (unexpected token))))

(defun as-property-name ()
  (if (member (token-type token) '(:num :string))
      (let ((result (copy-token (get-token-and-advance))))
        (setf (token-type result) :name)
        result)
      (as-name)))

(defun as-name ()
    (case (token-type token)
      (:name (get-token-and-advance))

      ((:operator :keyword :atom) 
       (let ((result (copy-token (get-token-and-advance))))
         (setf (token-type result) :name)
         result))

      (t (unexpected token))))

(defun subscripts (expr allow-calls)
  (cond 
    ((tokenp token :punc #\.)
     (next)
     (subscripts (as-binary-op expr :dot (as-name)) allow-calls))
    
    ((tokenp token :punc #\[)
     (let ((opening (get-token-and-advance))
           (sub (expression))
           (closing (expect #\])))
       (subscripts (as-binary-op expr :sub sub (sum-locations opening closing)) allow-calls)))
    
    ((and (tokenp token :punc #\() allow-calls)
     (let ((opening (get-token-and-advance))
           (args (expr-list #\)))
           (closing (get-token-and-advance)))
       (subscripts (as-binary-op expr :call args (sum-locations opening closing)) t)))
    
    ((and (unary-postfix-p token) allow-calls)
     (as-unary-postfix (get-token-and-advance) expr))

    (t expr)))

(defun expr-op (left min-prec)
  (let* ((op (and (token-type-p token :operator) (token-value token)))
         (prec (and op (gethash op *precedence*))))
    (if (and prec (> prec min-prec))
        (let ((right (progn (next) (expr-op (expr-atom t) prec))))
          (expr-op (as-binary-op left op right) min-prec))
        left)))

(defun expr-ops ()
  (expr-op (expr-atom t) 0))

(defun maybe-conditional ()
  (let ((expr (expr-ops)))
    (if (tokenp token :operator :?)
        (let ((yes (progn (next) (expr-ops))))
          (expect #\:)
          (as-conditional expr yes (expr-ops)))
        expr)))

(defun is-assignable (expr)
  (member (asn-type expr) '(:name :dot :sub)))

(defun maybe-assign ()
  (let ((left (maybe-conditional)))
    (if (assignment-p token)
        (if (is-assignable left)
            (as-binary-op left (get-token-and-advance) (maybe-assign))
            (error* "Invalid assignment."))
        left)))

(defun expression (&optional (commas t))
  (let ((expr (maybe-assign)))
    (if (and commas (tokenp token :punc #\,))
        (as-binary-op expr (get-token-and-advance) (expression))
        expr)))

(defun parse-js (token-source)
  (setf label-stack ()
        tokens token-source
        token-index 0
        peeked nil
        token (get-next-token t))
  (as-root (loop 
              :until (token-type-p token :eof)
              :collect (statement))))
