 
(in-package #:js-proc)

(defun parse-js-string (string &optional strict-semicolons)
  (declare (ignore string))
  ;; TODO--reimplement with new readers
)

(defun parse-filename (filename &optional strict-semicolons)
  (declare (ignore filename))
  ;; TODO--reimplement with new readers
)

(defstruct resource
  name
  path
  filename
  text
  raw-tokens
  folded-tokens
  ast
  doc
)

(defun get-resource-filename (path name)
  (concatenate 'string path (cl-ppcre:regex-replace-all "\\." name "/") ".js"))

(defun show-progress (item)
  (format t "~I~A~%" (resource-name item)))

;;
;; This is the document stack
;;
(defparameter *doc-items*
  (make-hash-table :test 'equal))

(defun make-doc-key (name type)
  (cons name type))

(defun append-doc (name item)
  (let ((key (make-doc-key name (doc-type item))))
    (if (gethash key *doc-items*)
        (format t "ERROR: multiple doc items for ~A~%." key)
        (setf (gethash key *doc-items*) item))))

(defun get-doc (name type &optional (create-p nil))
  (let* ((key (make-doc-key name  type))
         (item (gethash key *doc-items*)))
    (if item
        item
        (if create-p
            (setf (gethash key *doc-items*) (make-doc :type type))))))

(defun process-batch (batch)
  (let ((resources (make-array 100 :element-type 'resource :fill-pointer 0))
        (path (car batch)))

    (format t "reading...~%")
    (dolist (name (cdr batch))
      (let ((filename (get-resource-filename path name)))
        (format t "~I~A~%" name)
        (vector-push (make-resource :name name :filename filename :text (read-source-filename filename)) resources)))

    (format t "lexing...~%")
    (map nil (lambda (item) (show-progress item) (setf (resource-raw-tokens item) (lex (resource-text item)))) resources)

    (format t "folding comments...~%")
    (map nil (lambda (item) (show-progress item) (setf (resource-folded-tokens item) (fold-comments (resource-raw-tokens item)))) resources)

    (format t "parsing...~%")
    (map nil (lambda (item) (show-progress item) (setf (resource-ast item) (parse-js (resource-folded-tokens item)))) resources)

    (format t "traversing...~%")
    (map nil (lambda (item) 
               (show-progress item) 
               (doc-gen item #'append-doc #'get-doc))
         resources)

    (dump-doc-items *standard-output* *doc-items*)

    (format t "done...~%")

    resources))

(defun sources ()
  (cons "/usr/home/rcgill/dev/backdraft/src/"
        (list "bd"
              ;"bd.frenzy"
#|
              "bd.descriptor.processor"
              "bd.descriptor.cache"
              "bd.capture"
              "bd.command"
              "bd.frenzy"
              "bd.parentSpace"
              "bd.delayProcManager"
              "bd.namespace"
              "bd.types"
              "bd.data.lazyTreeStore"
              "bd.data.lazyTreeModel"
              "bd.data.dynaTreeModel"
              "bd.data.rowset"
              "bd.data.selector.ds.keyFilter"
              "bd.data.selector.ds.simpleFilter"
              "bd.data.selector.rs.simpleFilter"
              "bd.dijit.statusbar"
              "bd.dijit.pane"
              "bd.dijit.vScrollbar"
              "bd.dijit.scrollbar"
              "bd.dijit.group"
              "bd.dijit.staticText"
              "bd.dijit.textbox"
              "bd.dijit.radioGroup"
              "bd.dijit.hScrollbar"
              "bd.dijit.checkbox"
              "bd.dijit.combobox"
              "bd.dijit.root"
              "bd.dijit.labeledWidget"
              "bd.dijit.tree"
              "bd.dijit.borderContainer"
              "bd.dijit.tabContainer"
              "bd.dijit.dialog"
              "bd.dijit.listbox"
              "bd.dijit.console"
              "bd.dijit.contentPane"
              "bd.dijit.dateTextbox"
              "bd.dijit.button"
              "bd.dijit.verticalSlider"
              "bd.dijit.horizontalSlider"
1              "bd.dijit.mixin.core"
              "bd.dijit.mixin.container"
              "bd.dijit.mixin.navigator"
              "bd.dijit.menu"
              "bd.dijit.messagebox"
              "bd.test.mockXhr"
              "bd.test.matchers"
              "bd.test.mockFrenzyServer"
              "bd.test.moduleWrapper"
              "bd.test.result"
              "bd.test.space"
              "bd.test.publisher"
              "bd.test.loader"
              "bd.test.proc"
              "bd.resources.commandItems"
|#
              )))

(defun sourcesx ()
  (cons "/usr/home/rcgill/dev/js-proc/test/js-doc/"
        (list 
         "test08"
         )))
        
(defun test0 ()
  (clrhash *doc-items*)
  (process-batch (sources))
  nil
)
