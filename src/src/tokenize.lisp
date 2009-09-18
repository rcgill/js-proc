(in-package #:js-proc)

(defstruct location
  start-line
  start-char
  end-line
  end-char
)

(defstruct token 
  type 
  value
  location
  newline-before 
  comment
)

(defun tokenp (token type value)
  (and (eq (token-type token) type)
       (eql (token-value token) value)))
(defun token-type-p (token type)
  (eq (token-type token) type))
(defun token-id (token)
  (token-value token))
(defun token-line (token)
  (location-start-line (token-location token)))
(defun token-char (token)
  (location-start-char (token-location token)))

(defvar *line*)
(defvar *char*)

(define-condition js-parse-error (simple-error)
  ((line :initform *line* :reader js-parse-error-line)
   (char :initform *char* :reader js-parse-error-char)))

(defmethod print-object ((err js-parse-error) stream)
  (call-next-method)
  (format stream " (line ~a, character ~a)" (js-parse-error-line err) (js-parse-error-char err)))

(defun js-parse-error (control &rest args)
  (error 'js-parse-error :format-control control :format-arguments args))

(defparameter *hex-number* (cl-ppcre:create-scanner "^0x[0-9a-f]+$" :case-insensitive-mode t))
(defparameter *octal-number* (cl-ppcre:create-scanner "^0[0-7]+$"))
(defparameter *decimal-number* (cl-ppcre:create-scanner "^\\d*\\.?\\d*(?:e-?\\d*(?:\\d\\.?|\\.?\\d)\\d*)?$"
                                                        :case-insensitive-mode t))
(defparameter *operator-chars* "+-*&%=<>!?|~^")
(defparameter *operators*
  (let ((ops (make-hash-table :test 'equal)))
    (dolist (op '(:in :instanceof :typeof :new :void :delete :++ :-- :+ :- :! :~ :& :|\|| :^ :* :/ :%
                  :>> :<< :>>> :< :> :<= :>= :== :=== :!= :!== :? := :+= :-= :/= :*= :%= :>>= :<<=
                  :>>>= :~= :%= :|\|=| :^= :&& :|\|\||))
      (setf (gethash (string-downcase (string op)) ops) op))
    ops))

(defparameter *whitespace-chars* (concatenate 'string '(#\space #\tab #\return #\newline)))

(defparameter *keywords*
  (let ((keywords (make-hash-table :test 'equal)))
;;;;why isn't undefined a keyword?...because it isn't!
    (dolist (word '(:break :case :catch :continue :debugger :default :delete :do :else :false
                    :finally :for :function :if :in :instanceof :new :null :return :switch
                    :throw :true :try :typeof :var :void :while :with))
      (setf (gethash (string-downcase (string word)) keywords) word))
    (setf (gethash "NaN" keywords) :nan)
    keywords))
(defparameter *keywords-before-expression* '(:return :new :delete :throw))
(defparameter *atom-keywords* '(:false :null :true :undefined :nan))

;;;;(defun/defs lex-js0 (stream)
(defun/defs lex (text)
  (def regex-allowed t)
  (def newline-before nil)
  ;;;;(def line 1)
  (def line 0)
  (def char 0)
  (def line-count (length text))
  (def line-text (aref text 0))
  (def line-length (length line-text))

  (def start-token ()
    (setf *line* line
          *char* char))

  (def token (type value)
    (setf regex-allowed
          (or (eq type :operator)
              (and (eq type :keyword)
                   (member value *keywords-before-expression*))
              (and (eq type :punc)
                   (find value "[{}(,.;:"))))
    (prog1 (make-token 
            :type type 
            :value value 
            :location (make-location :start-line *line* :start-char *char* :end-line line :end-char char)
            :newline-before newline-before)
      (setf newline-before nil)))

  (def advance-line ()
    (setf line (1+ line) char 0 newline-before t line-text nil)
    (if (< line line-count)
        (setf line-text (aref text line) line-length (length line-text))))

  (def peek ()
    (cond
      ((eql line line-count) nil)
      ((eql char line-length) #\newline)
      (t (aref line-text char))))

  (def next (&optional eof-error)
    (cond
      ((eql line line-count) (if eof-error
                               (error 'end-of-file)
                               nil))
      ((eql char line-length) (progn (advance-line) #\newline))
      (t (prog1 (aref line-text char) (incf char)))))
    
    
  (def skip-whitespace ()
    (loop :for ch := (peek)
          :while (and ch (find ch *whitespace-chars*))
          :do (next)))

  (def read-while (pred)
    (with-output-to-string (*standard-output*)
      (loop :for ch := (peek)
            :while (and ch (funcall pred ch))
            :do (princ (next)))))

  (def read-num (start)
;;;;this is weak-- "a= -3-4;" fails; don't get why (eql ch #\1) is included
    (let ((num (read-while (lambda (ch) (or (alphanumericp ch) (eql ch #\.) (eql ch #\-))))))
      (when start (setf num (concatenate 'string start num)))
      (cond ((cl-ppcre:scan *hex-number* num)
             (token :num (parse-integer num :start 2 :radix 16)))
            ((cl-ppcre:scan *octal-number* num)
             (token :num (parse-integer num :start 1 :radix 8)))
            ((cl-ppcre:scan *decimal-number* num)
             (token :num (read-from-string num)))
            (t (js-parse-error "Invalid syntax: '~a'." num)))))

  (def handle-dot ()
    (next)
    (if (digit-char-p (peek))
        (read-num ".")
        (token :punc #\.)))

  (def hex-bytes (n)
    (loop :with num := 0
          :for pos :from (1- n) :downto 0
          :do (let ((digit (digit-char-p (next t) 16)))
                (if digit
                    (incf num (* digit (expt 16 pos)))
                    (js-parse-error "Invalid hex-character pattern in string.")))
          :finally (return num)))

  (def read-escaped-char ()
    (let ((ch (next t)))
      (case ch
        (#\n #\newline) (#\r #\return) (#\t #\tab)
        (#\b #\backspace) (#\v #\vt) (#\f #\page) (#\0 #\null)
        (#\x (code-char (hex-bytes 2)))
        (#\u (code-char (hex-bytes 4)))
        (t ch))))

  (def read-string ()
    (let ((quote (next)))
      (handler-case
          (token :string
                 (with-output-to-string (*standard-output*)
                   (loop (let ((ch (next t)))
                           (cond ((eql ch #\\) (write-char (read-escaped-char)))
                                 ((eql ch quote) (return))
                                 (t (write-char ch)))))))
        (end-of-file () (js-parse-error "Unterminated string constant.")))))


  (def read-line-comment ()
    (next)
    (token :comment
           (with-output-to-string (*standard-output*)
             (write-string "//")
             (loop :for ch := (next)
                :until (or (eql ch #\newline) (not ch))
                :do (write-char ch))
             )))

  (def read-multiline-comment ()
    (next)
    (token :block-comment
           (with-output-to-string (*standard-output*)
             (write-string "/*")
             (loop :with star := nil
                :for ch := (next)
                :until (or (not ch) (and star (eql ch #\/)))
                :do (write-char ch)
                :do (setf star (eql ch #\*))))))

  (def read-regexp ()
    (handler-case
        (token :regexp
               (cons
                (with-output-to-string (*standard-output*)
                  (loop :with backslash := nil
                        :for ch := (next t)
                        :until (and (not backslash) (eql ch #\/))
                        :do (setf backslash (and (eql ch #\\) (not backslash)))
                        :do (write-char (if (and (eql ch #\\) (eql (peek) #\u))
                                            (progn
                                              (setf backslash nil)
                                              (next)
                                              (code-char (hex-bytes 4)))
                                            ch))))
                (read-while (lambda (ch) (find ch "gim")))))
      (end-of-file () (js-parse-error "Unterminated regular expression."))))

  (def read-operator (&optional start)
    (labels ((grow (str)
               (let ((bigger (concatenate 'string str (string (peek)))))
                 (if (gethash bigger *operators*)
                     (progn (next) (grow bigger))
                     (token :operator (gethash str *operators*))))))
      (grow (or start (string (next))))))

  (def handle-slash ()
    (next)
    (case (peek)
      (#\/ (read-line-comment))
      (#\* (read-multiline-comment))
      (t (if regex-allowed
             (read-regexp)
             (read-operator "/")))))

  (def identifier-char-p (ch) (or (alphanumericp ch) (eql ch #\$) (eql ch #\_)))
  (def read-word ()
    (let* ((word (read-while #'identifier-char-p))
           (keyword (gethash word *keywords*)))
      (cond ((not keyword) (token :name word))
            ((gethash word *operators*) (token :operator keyword))
            ((member keyword *atom-keywords*) (token :atom keyword))
            (t (token :keyword keyword)))))

  (def next-token ()
    (skip-whitespace)
    (start-token)
    (let ((next (peek)))
      (cond ((not next) (token :eof "EOF"))
            ((digit-char-p next) (read-num nil))
            ((find next "'\"") (read-string))
            ((find next "[]{}(),.;:") (token :punc (next)))
            ((eql next #\.) (handle-dot))
            ((eql next #\/) (handle-slash))
            ((find next *operator-chars*) (read-operator))
            ((identifier-char-p next) (read-word))
            (t (js-parse-error "Unexpected character '~a'." next)))))

  (let ((tokens (make-array 1000 :element-type 'token :fill-pointer 0 :adjustable t)))
    (do ((token (next-token) (next-token)))
        ((token-type-p token :eof) (vector-push-extend token tokens 1))
      (vector-push-extend token tokens 1000))
    tokens
    ))

(defun fold-comments (tokens)
  (let* ((i -1)
         (token-count (length tokens))
         (result (make-array token-count :element-type 'token :fill-pointer 0))
         (last-non-comment-token-line -2))
    (labels 
        ((next () 
           (aref tokens (incf i)))

         (get-comment-count (start-line)
           (do* ((i (1+ i))
                 (token (aref tokens i) (aref tokens (incf i)))
                 (line (1+ start-line) (incf line)))
                ((or (not (token-type-p token :comment)) (/= (token-line token) line)) (- line start-line)))))

      (do ((token (next) (next)))
          ((= i token-count) result)
        (if (token-type-p token :comment)
            ;aggregate all the comments that are on contiguous lines into one value
            ;if they start on the same or next line as another type of token, then fold into that token
            ;otherwise, make them their own token
            ;note: currently, block-comments are not considered
            (let* ((start-line (token-line token))
                   (start-char (token-char token))
                   (start-newline-before (token-newline-before token)) ;this appears to be useless, but we carry it just in case is becomes useful
                   (end-line (token-line token))
                   (end-char (location-end-char (token-location token)))
                   (comment-count (get-comment-count start-line))
                   (comment (make-array comment-count :element-type 'string :fill-pointer 0)))
              (vector-push (token-value token) comment)
              (dotimes (i (1- comment-count))
                (setf 
                 token (next) 
                 end-line (token-line token)
                 end-char (location-end-char (token-location token)))
                (vector-push (token-value token) comment))
              (if (>= (1+ last-non-comment-token-line) start-line) 
                  (setf (token-comment (aref result (1- (fill-pointer result)))) comment)
                  (vector-push (make-token 
                                :type :comment 
                                :value comment 
                                :location (make-location :start-line start-line :start-char start-char :end-line end-line :end-char end-char)
                                :newline-before start-newline-before) result)))
            (progn (setf last-non-comment-token-line (token-line token)) (vector-push token result)))))))
