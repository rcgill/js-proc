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
  (references (make-hash-table :test 'equal))
  (requires (make-hash-table))
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

(defun write-resource-header (resource)
  (with-open-file (out (resource-filename resource) :direction :output :if-exists :supersede)
    (format out "dojo.provide(\"~A\");~%" (resource-name resource))
    (let ((requires (resource-requires resource)))
      (maphash (lambda (key value)
                 (declare (ignore value))
                 (format out "dojo.require(\"~A\");~%" (resource-name key))) requires))
    (format out "///file~%//~%~%")
    (map nil (lambda (line) (format out "~A~%" line)) (resource-text resource))
    (format out "// Copyright (c) 2000-2009, Altoviso, Inc. (www.altoviso.com). Use, modification, and distribution subject to terms of license.~%")))

(defun process-batch (batch)
  (let ((resources (make-array 100 :element-type 'resource :fill-pointer 0))
        (path (car batch))
        init-resource)

    (format t "reading...~%")
    (dolist (name (cdr batch))
      (let ((filename (get-resource-filename path name)))
        (format t "~I~A~%" name)
        (let ((resource (make-resource :name name :filename filename :text (read-source-filename filename))))
          (if (equal name "bd.init")
              (setf init-resource resource))
          (vector-push resource resources))))

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
      (with-open-file (out "/home/rcgill/dev/backdraft/doc/generated/com.altoviso.backdraft.api.ref.js" :direction :output :if-exists :supersede)
        (format out "~A~%" prefix)
        (dump-doc-items-to-json out *doc-items*)
        (format out "~A~%" suffix)))

#|
    (format t "dumping dependencies...~%")
    (let ((defines (make-hash-table :test 'equal))
          (not-found (make-hash-table :test 'equal)))
      (map nil (lambda (resource) 
                 (show-progress resource)
                 (find-defines-and-references resource defines))
           resources)
      
      (map nil (lambda (resource)
                 (let ((requires (resource-requires resource)))
                   (if (not (eq resource init-resource))
                       (setf (gethash init-resource requires) t))
                   (maphash (lambda (key value)
                              (declare (ignore value))
                              (let ((src (gethash key defines)))
                                (if src
                                    (setf (gethash src requires) t)
                                    (setf (gethash key not-found) t))))
                            (resource-references resource))
                   (write-resource-header resource)))
           resources)
      (format t "the following symbols were not defined...")
      (maphash (lambda (key value)
                 (declare (ignore value))
                 (format t "~A~%" key)) not-found))
|#
    (format t "done...~%")

    resources))

(defun sources ()
  (cons "/usr/home/rcgill/dev/backdraft/src/"
        '(
"bd"
"bd/async"
;"bd/buildTools"
"bd/clone"
"bd/collections"
"bd/command/namespace"
"bd/command/accelerators"
"bd/command/dispatch"
"bd/command/item"
"bd/connect"
"bd/connectable"
"bd/containable"
"bd/container"
"bd/creators"
"bd/css"
"bd/cssStateful"
;"bd/data/dynaTreeModel"
;"bd/data/lazyTreeModel"
;"bd/data/lazyTreeStore"
;"bd/data/rowset"
;"bd/data/selector/ds/keyFilter"
;"bd/data/selector/ds/simpleFilter"
;"bd/data/selector/rs/simpleFilter"
"bd/declare"
"bd/descriptor/cache"
"bd/descriptor/processor"
"bd/dijit/accordionContainer"
"bd/dijit/accordionPane"
"bd/dijit/button"
"bd/dijit/calendar"
"bd/dijit/checkBox"
"bd/dijit/colorPalette"
"bd/dijit/comboBox"
"bd/dijit/comboButton"
"bd/dijit/compat"
"bd/dijit/contentPane"
"bd/dijit/currencyTextBox"
"bd/dijit/dateTextBox"
"bd/dijit/dropDownButton"
"bd/dijit/editor"
"bd/dijit/filteringSelect"
"bd/dijit/horizontalRule"
"bd/dijit/horizontalRuleLabels"
"bd/dijit/horizontalSlider"
"bd/dijit/inlineEditBox"
"bd/dijit/layoutContainer"
"bd/dijit/linkPane"
"bd/dijit/mappedTextBox"
"bd/dijit/multiSelect"
"bd/dijit/numberSpinner"
"bd/dijit/numberTextBox"
"bd/dijit/progressBar"
"bd/dijit/radioButton"
"bd/dijit/rangeBoundTextBox"
"bd/dijit/scrollingTabController"
"bd/dijit/select"
"bd/dijit/simpleTextArea"
"bd/dijit/stackContainer"
"bd/dijit/stackController"
"bd/dijit/tabContainer"
"bd/dijit/tabController"
"bd/dijit/textArea"
"bd/dijit/textBox"
"bd/dijit/timeTextBox"
"bd/dijit/titlePane"
"bd/dijit/toggleButton"
"bd/dijit/toolbar"
"bd/dijit/toolbarSeparator"
"bd/dijit/tooltip"
"bd/dijit/tree"
"bd/dijit/validationTextBox"
"bd/dijit/verticalRule"
"bd/dijit/verticalRuleLabels"
"bd/dijit/verticalSlider"
"bd/dom"
"bd/equal"
"bd/focusable"
"bd/frenzy"
"bd/hash"
"bd/htmlGen"
"bd/id"
"bd/interactive"
"bd/kernel"
"bd/lang"
"bd/mouse"
"bd/mouseable"
"bd/namespace"
"bd/navigator"
;"bd/nls/command"
"bd/start"
"bd/stateful"
"bd/string"
"bd/symbols"
"bd/test"
;"bd/test/loader"
"bd/test/matchers"
"bd/test/mockFrenzyServer"
"bd/test/mockXhr"
"bd/test/namespace"
;"bd/test/proc"
;"bd/test/publisher"
;"bd/test/result"
;"bd/test/robot"
;"bd/test/space"
"bd/visual"
"bd/widget/borderContainer"
"bd/widget/checkBox"
"bd/widget/console"
"bd/widget/dialog"
"bd/widget/iframe"
"bd/widget/labeled"
;"bd/widget/listbox"
"bd/widget/menu"
"bd/widget/messageBox"
"bd/widget/pane"
"bd/widget/radioGroup"
"bd/widget/root"
"bd/widget/stateButton"
"bd/widget/staticText"
"bd/widget/statusbar"
          )))

(defun test0 ()
  (clrhash *doc-items*)
  (process-batch (sources))
  nil
)

(defun backdraft-sources ()
(let ((raw 
"./bd/capture.js
./bd/collections.js
./bd/command/accelerators.js
./bd/command/dispatch.js
./bd/command/item.js
./bd/command.js
./bd/css.js
./bd/data/dynaTreeModel.js
./bd/data/lazyTreeModel.js
./bd/data/rowset.js
./bd/data/selector/ds/keyFilter.js
./bd/data/selector/ds/simpleFilter.js
./bd/data/selector/rs/simpleFilter.js
./bd/async.js
./bd/descriptor/cache.js
./bd/descriptor/processor.js
./bd/dijit/borderContainer.js
./bd/dijit/button.js
./bd/dijit/checkbox.js
./bd/dijit/combobox.js
./bd/dijit/console.js
./bd/dijit/contentPane.js
./bd/dijit/dateTextbox.js
./bd/dijit/dialog.js
./bd/dijit/group.js
./bd/dijit/horizontalSlider.js
./bd/dijit/hScrollbar.js
./bd/dijit/labeledWidget.js
./bd/dijit/listbox.js
./bd/dijit/menu.js
./bd/dijit/messagebox.js
./bd/dijit/mixin/container.js
./bd/dijit/mixin/core.js
./bd/dijit/mixin/navigator.js
./bd/dijit/pane.js
./bd/dijit/radioGroup.js
./bd/dijit/root.js
./bd/dijit/scrollbar.js
./bd/dijit/staticText.js
./bd/dijit/statusbar.js
./bd/dijit/tabContainer.js
./bd/dijit/textbox.js
./bd/dijit/tree.js
./bd/dijit/verticalSlider.js
./bd/dijit/vScrollbar.js
./bd/frenzy.js
./bd/lang.js
./bd/namespace.js
./bd/parentSpace.js
./bd/resources/commandItems.js
./bd/test/loader.js
./bd/test/matchers.js
./bd/test/mockFrenzyServer.js
./bd/test/mockXhr.js
./bd/test/moduleWrapper.js
./bd/test/proc.js
./bd/test/publisher.js
./bd/test/result.js
./bd/test/space.js
./bd/test.js
./bd/types.js
./bd/init.js
./bd/start.js
./bd/symbols.js
./bd/loader.js"))
  (setf raw (cl-ppcre:regex-replace-all "\\./" raw ""))
  (setf raw (cl-ppcre:regex-replace-all "\\.js" raw ""))
  (setf raw (cl-ppcre:regex-replace-all "/" raw "."))
  (setf raw (cl-ppcre:split "\\n" raw))
  (setf raw (remove "bd.test.moduleWrapper" raw :test 'equal))
raw
))
;;TODO add files to ignore
;;TODO don't delete dojo.requires from modules that are not part of the scan
;;e.g., don'te delete dojo modules when only checking backdraft dependencies