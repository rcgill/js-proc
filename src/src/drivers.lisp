 
(in-package #:js-proc)

(defun parse-js-string (string &optional strict-semicolons)
  ;; TODO--reimplement with new readers
)

(defun parse-filename (filename &optional strict-semicolons)
  ;; TODO--reimplement with new readers
)

(defstruct resource-ctrl
  resource
  path
  filename
  text
  (raw-tokens nil)
  (folded-tokens nil)
  (ast nil)
)

(defun get-resource-filename (path resource)
  (concatenate 'string path (cl-ppcre:regex-replace-all "\\." resource "/") ".js"))

(defun show-progress (item)
  (format t "~I~A~%" (resource-ctrl-resource item)))


(defparameter *current-source* nil)

(defun process-batch (batch)
  (let ((resources (make-array 100 :element-type 'resource-ctrl :fill-pointer 0))
        (path (car batch)))

    (format t "reading...~%")
    (dolist (resource (cdr batch))
      (let ((filename (get-resource-filename path resource)))
        (format t "~I~A~%" resource)
        (vector-push (make-resource-ctrl :resource resource :filename filename :text (read-source-filename filename)) resources)))

    (format t "lexing...~%")
    (map nil (lambda (item) (show-progress item) (setf (resource-ctrl-raw-tokens item) (lex (resource-ctrl-text item)))) resources)

    (format t "folding comments...~%")
    (map nil (lambda (item) (show-progress item) (setf (resource-ctrl-folded-tokens item) (fold-comments (resource-ctrl-raw-tokens item)))) resources)

    (format t "parsing...~%")
    (map nil (lambda (item) (show-progress item) (setf (resource-ctrl-ast item) (parse-js (resource-ctrl-folded-tokens item)))) resources)

    (format t "traversing...~%")
    (map nil (lambda (item) 
               (show-progress item) 
               (setf *current-source* item)
               (generate-docs (resource-ctrl-ast item)))
         resources)

    (format t "done...~%")

    (setf *current-source* nil)
    resources))

(defun sources ()
  (cons "/usr/home/rcgill/dev/backdraft/src/"
        (list "bd2"
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
(              "bd.dijit.hScrollbar"
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
              "bd.dijit.mixin.core"
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
    
(defun test0 ()
  (clrhash *doc-items*)
  (process-batch (sources))
  nil
)
