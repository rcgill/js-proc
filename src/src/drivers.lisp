 
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

(defun get-wrapper-file (filename)
  (with-open-file (in filename)
    (let ((top (read-line in))
          (bottom nil))
      (do ((line (read-line in) (read-line in nil)))
          ((not line) (values top bottom))
        (if (not bottom)
            (if (equal line "//CONTENTS")
                (setf bottom (read-line in))
                (setf top (concatenate 'string top new-line-str line)))
            (setf bottom (concatenate 'string bottom new-line-str line)))))))

;;
;; This is the document stack
;;
(defparameter *doc-items*
  (make-hash-table :test 'equal))

(defun make-doc-key (name type)
  (cons name type))

(defun process-batch (batch)
  (let ((resources (make-array 100 :element-type 'resource :fill-pointer 0))
        (path (car batch)))

    (format t "reading...~%")
    (dolist (name (cdr batch))
      (let ((filename (get-resource-filename path name)))
        (format t "~I~A~%" name)
        (vector-push (make-resource :name name :filename filename :text (read-source-filename filename)) resources)))

    (format t "lexing...~%")
    (map nil (lambda (resource) (show-progress resource) (setf (resource-raw-tokens resource) (lex (resource-text resource)))) resources)

    (format t "folding comments...~%")
    (map nil (lambda (resource) (show-progress resource) (setf (resource-folded-tokens resource) (fold-comments (resource-raw-tokens resource)))) resources)

    (format t "parsing...~%")
    (map nil (lambda (resource) (show-progress resource) (setf (resource-ast resource) (parse-js (resource-folded-tokens resource)))) resources)

    (format t "traversing...~%")
    (map nil (lambda (resource) 
               (show-progress resource)
               (labels ((append-doc-for-this-resource (name doc)
                          (setf (doc-source doc) resource)
                          (let ((key (make-doc-key name (doc-type doc))))
                            (if (gethash key *doc-items*)
                                (format t "ERROR: multiple doc items for ~A~%." key)
                                (setf (gethash key *doc-items*) doc))))

                        (get-doc (name type &optional (create-p nil))
                          (let* ((key (make-doc-key name type))
                                 (doc (gethash key *doc-items*)))
                            (or doc (and create-p (setf (gethash key *doc-items*) (make-doc :type type :source resource)))))))
                 (doc-gen resource #'append-doc-for-this-resource #'get-doc)))
         resources)

    (multiple-value-bind (prefix suffix) (get-wrapper-file "/home/rcgill/dev/backdraft/doc/com.altoviso.backdraft.api.ref.main.js")
      (with-open-file (out "/home/rcgill/dev/backdraft/doc/generated/com.altoviso.backdraft.api.ref.main.js" :direction :output :if-exists :supersede)
        (format out "~A~%" prefix)
        (dump-doc-items-to-json out *doc-items*)
        (format out "~A~%" suffix)))

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
