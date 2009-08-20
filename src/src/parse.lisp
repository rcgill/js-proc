(in-package #:js-proc)

(defparameter *unary-prefix* '(:typeof :void :delete :-- :++ :! :~ :- :+))
(defparameter *unary-postfix* '(:-- :++))
(defparameter *assignment*
  (let ((assign (make-hash-table)))
    (dolist (op '(:+= :-= :/= :*= :%= :>>= :<<= :>>>= :~= :%= :|\|=| :^=))
      (setf (gethash op assign) (intern (subseq (string op) 0 (1- (length (string op)))) :keyword)))
    (setf (gethash := assign) t)
    assign))

(defparameter *precedence*
  (let ((precs (make-hash-table)))
    (loop :for ops :in '((:|\|\||) (:&&) (:|\||) (:^) (:&) (:== :=== :!= :!==)
                         (:< :> :<= :>= :in :instanceof) (:>> :<< :>>>) (:+ :-) (:* :/ :%))
          :for n :from 1
          :do (dolist (op ops) (setf (gethash op precs) n)))
    precs))

(defparameter *in-function* nil)
(defparameter *in-loop* nil)

(defstruct node-info
  type     ;(symbol) the type of the production
  comment  ;(vector of string) the comment associated with the production
  location ;(quadruple as a list) (start-line start-char end-line end-char) the location in the source for the production
)

;(defun/defs parse-js (stream &optional strict-semicolons)
;  (def input (lex-js stream))
;  (def token (funcall input))
(defun/defs parse-js (tokens &optional strict-semicolons)
  (def line 0)
  (def get-next-token ()
    (prog1 (aref tokens line) (incf line)))
  (def token (get-next-token))
  (def peeked nil)

  (def peek ()
    (or peeked (setf peeked (get-next-token))))
  (def next ()
    (if peeked
        (setf token peeked peeked nil)
        (setf token (get-next-token)))
    token)
  (def skip (n)
    (dotimes (i n) (next)))

  (def token-error (token control &rest args)
    (let ((*line* (token-line token)) (*char* (token-char token)))
      (apply #'js-parse-error control args)))
  (def error* (control &rest args)
    (apply #'token-error token control args))
  (def unexpected (token)
    (token-error token "Unexpected token '~a'." (token-id token)))

  (def expect-token (type val)
    (if (tokenp token type val)
        (prog1 (token-comment token) (next))
        (error* "Unexpected token '~a', expected '~a'." (token-id token) val)))
  (def expect (punc)
    (expect-token :punc punc))
  (def expect-key (keyword)
    (expect-token :keyword keyword))
  (def semicolon ()
    (cond (strict-semicolons (expect #\;))
          ((tokenp token :punc #\;) (next))
          ((not (token-newline-before token)) (unexpected token))))

  (def as (type &rest args)
    (cons (make-node-info :type type) args))

  (def as-with-comment (type comment &rest args)
    (cons (make-node-info :type type :comment comment) args))

  (def labels ())

  (def parenthesised ()
    (expect #\() (prog1 (expression) (expect #\))))

  (def maybe-before-semicolon (func)
    (if (tokenp token :punc #\;)
        (prog1 nil (next))
        (let ((start token))
          (handler-case (prog1 (funcall func) (semicolon))
            (js-parse-error (err)
              (unless (and (eq token start) (token-newline-before token) (not strict-semicolons)) (error err))
              nil)))))

  (def statement (&optional allow-case)
    (case (token-type token)
      (:comment (prog1 (as :comment token) (next)))
      ((:num :string :regexp :operator :atom) (simple-statement))
      (:name (if (tokenp (peek) :punc #\:)
                 (labeled-statement (prog1 (token-value token) (skip 2)))
                 (simple-statement)))
      (:punc (case (token-value token)
               (#\{ (next) (block*))
               ((#\[ #\() (simple-statement))
               (#\; (next) (as :block))
               (t (unexpected token))))
      (:keyword
       (case (prog1 (token-value token) (next))
         (:break (break/cont :break))
         (:case (unless allow-case (unexpected token))
                (let ((val (expression)))
                  (expect #\:)
                  (as :case val)))
         (:continue (break/cont :continue))
         (:debugger (semicolon) (as :debugger))
         (:default (unless allow-case (unexpected token))
                   (expect #\:)
                   (as :default))
         (:do (let ((body (let ((*in-loop* t)) (statement))))
                (expect-key :while)
                (let ((condition (parenthesised)))
                  (semicolon)
                  (as :do condition body))))
         (:for (for*))
         (:function (function* t))
         (:if (if*))
         (:return (unless *in-function* (error* "'return' outside of function."))
                  (as :return (maybe-before-semicolon #'expression)))
         (:switch (let ((val (parenthesised)))
                    (expect #\{)
                    (let ((body (let ((*in-loop* t))
                                  (loop :until (tokenp token :punc #\})
                                        :collect (statement t)))))
                      (next)
                      (as :switch val body))))
         (:throw (let ((ex (expression))) (semicolon) (as :throw ex)))
         (:try (try*))
         (:var (prog1 (var*) (semicolon)))
         (:while (as :while (parenthesised) (let ((*in-loop* t)) (statement))))
         (:with (as :with (parenthesised) (statement)))
         (t (unexpected token))))
      (t (unexpected token))))

  (def labeled-statement (label)
    (push label labels)
    (let ((start token)
          (stat (statement)))
      (unless (member (car stat) '(:for :do :while :switch)) (unexpected start))
      (pop labels)
      (as :label label stat)))

  (def simple-statement ()
    (let ((exp (expression)))
      (semicolon)
      (as :stat exp)))

  (def break/cont (type)
    (unless *in-loop* (error* "'~a' not inside a loop or switch." type))
    (let ((name nil))
      (when (token-type-p token :name)
        (setf name (token-value token))
        (next)
        (unless (member name labels :test #'string=)
          (error* "Labeled '~a' without matching loop or switch statement." type)))
      (semicolon)
      (as type name)))

  (def block* ()
    (prog1 (as :block (loop :until (tokenp token :punc #\})
                            :collect (statement)))
      (next)))

  (def for* ()
    (expect #\()
    (let ((var (tokenp token :keyword :var)))
      (when var (next))
      (if (and (token-type-p token :name) (tokenp (peek) :operator :in))
          (let ((name (token-value token)))
            (skip 2)
            (let ((obj (expression)))
              (expect #\))
;;;;don't see the point of var in the next production
              (as :for-in var name obj (let ((*in-loop* t)) (statement)))))
          (let ((init (maybe-before-semicolon (if var #'var* #'expression)))
                (test (maybe-before-semicolon #'expression))
                (step (if (tokenp token :punc #\)) nil (expression))))
            (expect #\))
            (as :for init test step (let ((*in-loop* t)) (statement)))))))

  (def function* (statement)
    (with-defs
      (def name (and (token-type-p token :name)
                     (prog1 (token-value token) (next))))
      (when (and statement (not name)) (unexpected token))
      (expect #\()
      ;(def argnames (loop :for first := t :then nil
      ;                    :until (tokenp token :punc #\))
      ;                    :unless first :do (expect #\,)
      ;                    :unless (token-type-p token :name) :do (unexpected token)
      ;                    :collect (prog1 (token-value token) (next))))

      (def argnames (do ((params nil))
                        ((tokenp token :punc #\)) (reverse params))
                      (if (token-type-p token :name)
                          (let ((name (token-value token))
                                (comment (token-comment token)))
                            (next)
                            (if (tokenp token :punc #\,)
                                (progn 
                                  (setf comment (token-comment token))
                                  (next)))
                            (push (cons name comment) params))
                          (unexpected token))))
      

      (next)
      (def comment (expect #\{))
      (def body (let ((*in-function* t) (*in-loop* nil))
                  (loop :until (tokenp token :punc #\})
                        :collect (statement))))
      (next)
      (as-with-comment (if statement :defun :function) comment name argnames body)))

  (def if* ()
    (let ((condition (parenthesised))
          (body (statement))
          else)
      (when (tokenp token :keyword :else)
        (next)
        (setf else (statement)))
      (as :if condition body else)))
      
  (def try* ()
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
      (as :try body catch finally)))

  (def vardefs ()
    (unless (token-type-p token :name) (unexpected token))
    (let ((name (token-value token)) val)
      (next)
      (when (tokenp token :operator :=)
        (next) (setf val (expression)))
      (if (tokenp token :punc #\,)
          (progn (next) (cons (cons name val) (vardefs)))
          (list (cons name val)))))

  (def var* ()
    (as :var (vardefs)))

  (def new* ()
    (let ((newexp (expr-atom nil)))
      (let ((args nil))
        (when (tokenp token :punc #\()
          (next) (setf args (expr-list #\))))
        (subscripts (as :new newexp args) t))))

  (def expr-atom (allow-calls)
    (cond ((tokenp token :operator :new) (next) (new*))
          ((and (token-type-p token :operator) (member (token-value token) *unary-prefix*))
           (make-unary :unary-prefix (prog1 (token-value token) (next)) (expr-atom allow-calls)))
          ((token-type-p token :punc)
           (case (token-value token)
             (#\( (next) (subscripts (prog1 (expression) (expect #\))) allow-calls))
             (#\[ (next) (subscripts (array*) allow-calls))
             (#\{ (subscripts (object*) allow-calls))
             (t (unexpected token))))
          ((tokenp token :keyword :function)
           (next)
           (function* nil))
          ((member (token-type token) '(:atom :num :string :regexp :name))
           (subscripts (prog1 (as (token-type token) (token-value token)) (next)) allow-calls))
          (t (unexpected token))))

  (def expr-list (closing)
    (prog1 (loop :for first := t :then nil
                 :until (tokenp token :punc closing)
                 :unless first :do (expect #\,)
                 :collect (expression nil))
      (next)))

  (def array* ()
    (as :array (expr-list #\])))

  (def object* ()
    (let ((comment (token-comment token)))
      (next)
      (as-with-comment :object comment
          (loop :for first := t :then nil
                     :until (tokenp token :punc #\})
                     :unless first :do (progn (expect #\,) (when (token-type-p token :comment) (next)))
                     :collect (list (as-property-name) (expect #\:) (expression nil))
                     :finally (next)))))

  (def as-property-name ()
    (if (member (token-type token) '(:num :string))
        (prog1 (token-value token) (next))
        (as-name)))

  (def as-name ()
    (case (token-type token)
      (:name (prog1 (token-value token) (next)))
      ((:operator :keyword :atom) (prog1 (symbol-name (token-value token)) (next)))
      (t (unexpected token))))

  (def subscripts (expr allow-calls)
    (cond ((tokenp token :punc #\.)
           (next)
           (subscripts (as :dot expr (as-name)) allow-calls))
          ((tokenp token :punc #\[)
           (next)
           (let ((sub (expression)))
             (expect #\])
             (subscripts (as :sub expr sub) allow-calls)))
          ((and (tokenp token :punc #\() allow-calls)
           (next)
           (let ((args (expr-list #\))))
             (subscripts (as :call expr args) t)))
          ((and (token-type-p token :operator) (member (token-value token) *unary-postfix*) allow-calls)
           (prog1 (make-unary :unary-postfix (token-value token) expr) (next)))
          (t expr)))

  (def make-unary (tag op expr)
    (when (and (member op '(:++ :-- :delete)) (not (is-assignable expr)))
      (error* "Invalid use of '~a' operator." op))
    (as tag op expr))

  (def expr-op (left min-prec)
    (let* ((op (and (token-type-p token :operator) (token-value token)))
           (prec (and op (gethash op *precedence*))))
      (if (and prec (> prec min-prec))
          (let ((right (progn (next) (expr-op (expr-atom t) prec))))
            (expr-op (as :binary op left right) min-prec))
          left)))

  (def expr-ops ()
    (expr-op (expr-atom t) 0))

  (def maybe-conditional ()
    (let ((expr (expr-ops)))
      (if (tokenp token :operator :?)
          (let ((yes (progn (next) (expr-ops))))
            (expect #\:)
            (as :conditional expr yes (expr-ops)))
          expr)))

  (def is-assignable (expr)
    ;(member (car expr) '(:name :dot :sub)))
    (member (node-info-type (car expr)) '(:name :dot :sub)))

  (def maybe-assign ()
    (let ((left (maybe-conditional)))
      (if (and (token-type-p token :operator) (gethash (token-value token) *assignment*))
          (if (is-assignable left)
              (let ((assign-type (gethash (token-value token) *assignment*))
                    (comment (token-comment token)))
                (next)
                (as-with-comment :assign comment assign-type left (maybe-assign)))
              (error* "Invalid assignment."))
          left)))

  (def expression (&optional (commas t))
    (let ((expr (maybe-assign)))
      (if (and commas (tokenp token :punc #\,))
          (as :seq expr (progn (next) (when (token-type-p token :comment) (next)) (expression)))
          expr)))

  (as :toplevel (loop :until (token-type-p token :eof)
                      :collect (statement))))
