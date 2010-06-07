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
"bd/test/loader"
"bd/test/matchers"
"bd/test/mockFrenzyServer"
"bd/test/mockXhr"
"bd/test/namespace"
"bd/test/proc"
"bd/test/publisher"
"bd/test/result"
;"bd/test/robot"
"bd/test/space"
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

(defparameter docder-sources
  '(
    "/dojo/dojo/dojo-ng.js"
    "/dojo/dojo/_base/version.js"
    "/dojo/dojo/_base/backCompat.js"
    "/dojo/dojo/_base/kernel.js"
    "/dojo/dojo/_base/environment/browser.js"
    "/dojo/dojo/_base/eval.js"
    "/dojo/dojo/_base/lang.js"
    "/dojo/dojo/_base/array.js"
    "/dojo/dojo/_base/unloadDetect.js"
    "/dojo/dojo/_base/xhr.js"
    "/dojo/dojo/_base/Color.js"
    "/dojo/dojo/_base/declare.js"
    "/dojo/dojo/_base/Deferred.js"
    "/dojo/dojo/_base/json.js"
    "/dojo/dojo/_base/window.js"
    "/dojo/dojo/_base/connect.js"
    "/dojo/dojo/_base/event.js"
    "/dojo/dojo/_base/html.js"
    "/dojo/dojo/_base/NodeList.js"
    "/dojo/dojo/_base/query.js"
    "/dojo/dojo/_base/fx.js"
    "/docder-dev/js/docder.js"
    "/bd/src/bd.js"
    "/bd/src/bd/command/accelerators.js"
    "/bd/src/bd/widget/menu.js"
    "/docder-dev/js/docder/docManager.js"
    "/docder-dev/js/docder/util.js"
    "/docder-dev/js/docder/command.js"
    "/docder-dev/js/docder/search.js"
    "/docder-dev/js/docder/paneManager.js"
    "/docder-dev/js/docder/showdown.js"
    "/docder-dev/js/docder/pageTypes/catalogPage.js"
    "/docder-dev/js/docder/pageTypes/document.js"
    "/docder-dev/js/docder/pageTypes/generic.js"
    "/bd/src/bd/kernel.js"
    "/bd/src/bd/lang.js"
    "/bd/src/bd/declare.js"
    "/bd/src/bd/hash.js"
    "/bd/src/bd/clone.js"
    "/bd/src/bd/equal.js"
    "/bd/src/bd/connect.js"
    "/bd/src/bd/collections.js"
    "/bd/src/bd/string.js"
    "/bd/src/bd/creators.js"
    "/bd/src/bd/start.js"
    "/bd/src/bd/command/namespace.js"
    "/bd/src/bd/command/item.js"
    "/bd/src/bd/command/dispatch.js"
    "/bd/src/bd/stateful.js"
    "/bd/src/bd/containable.js"
    "/bd/src/bd/dijit/compat.js"
    "/dojo/dijit/CheckedMenuItem.js"
    "/dojo/dijit/Menu.js"
    "/dojo/dijit/MenuBar.js"
    "/dojo/dijit/MenuBarItem.js"
    "/dojo/dijit/MenuSeparator.js"
    "/dojo/dijit/PopupMenuBarItem.js"
    "/dojo/dijit/PopupMenuItem.js"
    "/bd/src/bd/symbols.js"
    "/bd/src/bd/widget/messageBox.js"
    "/dojo/dojo/_base/i18n.js"
    "/dojo/dojo/data/ItemFileWriteStore.js"
    "/bd/src/bd/widget/staticText.js"
    "/bd/src/bd/dijit/comboBox.js"
    "/bd/src/bd/widget/radioGroup.js"
    "/bd/src/bd/widget/checkBox.js"
    "/bd/src/bd/descriptor/processor.js"
    "/bd/src/bd/widget/pane.js"
    "/dojo/dijit/_base.js"
    "/bd/src/bd/namespace.js"
    "/bd/src/bd/async.js"
    "/dojo/dijit/MenuItem.js"
    "/dojo/dojo/window.js"
    "/dojo/dijit/_Widget.js"
    "/dojo/dijit/_KeyNavContainer.js"
    "/dojo/dijit/_Templated.js"
    "/dojo/dijit/_Contained.js"
    "/bd/src/bd/widget/dialog.js"
    "/bd/src/bd/dijit/button.js"
    "/dojo/dojo/data/ItemFileReadStore.js"
    "/bd/src/bd/visual.js"
    "/dojo/dijit/form/ComboBox.js"
    "/bd/src/bd/widget/labeled.js"
    "/bd/src/bd/css.js"
    "/bd/src/bd/widget/stateButton.js"
    "/bd/src/bd/container.js"
    "/bd/src/bd/navigator.js"
    "/dojo/dijit/_base/focus.js"
    "/dojo/dijit/_base/manager.js"
    "/dojo/dijit/_base/place.js"
    "/dojo/dijit/_base/popup.js"
    "/dojo/dijit/_base/scroll.js"
    "/dojo/dijit/_base/sniff.js"
    "/dojo/dijit/_base/typematic.js"
    "/dojo/dijit/_base/wai.js"
    "/dojo/dijit/_base/window.js"
    "/bd/src/bd/dom.js"
    "/dojo/dijit/_CssStateMixin.js"
    "/dojo/dijit/_Container.js"
    "/dojo/dojo/string.js"
    "/dojo/dojo/parser.js"
    "/dojo/dojo/cache.js"
    "/bd/src/bd/focusable.js"
    "/bd/src/bd/mouseable.js"
    "/bd/src/bd/htmlGen.js"
    "/bd/src/bd/mouse.js"
    "/dojo/dojo/dnd/Moveable.js"
    "/dojo/dojo/dnd/TimedMoveable.js"
    "/dojo/dijit/form/Button.js"
    "/dojo/dojo/data/util/filter.js"
    "/dojo/dojo/data/util/simpleFetch.js"
    "/dojo/dojo/date/stamp.js"
    "/bd/src/bd/id.js"
    "/bd/src/bd/connectable.js"
    "/bd/src/bd/cssStateful.js"
    "/dojo/dojo/regexp.js"
    "/dojo/dijit/form/_FormWidget.js"
    "/dojo/dijit/form/ValidationTextBox.js"
    "/dojo/dojo/AdapterRegistry.js"
    "/dojo/dojo/uacss.js"
    "/dojo/dojo/_base/text.js"
    "/bd/src/bd/interactive.js"
    "/dojo/dojo/dnd/Mover.js"
    "/dojo/dijit/_HasDropDown.js"
    "/dojo/dojo/data/util/sorter.js"
    "/dojo/dojo/i18n.js"
    "/dojo/dijit/form/TextBox.js"
    "/dojo/dijit/Tooltip.js"
    "/dojo/dojo/dnd/common.js"
    "/dojo/dojo/dnd/autoscroll.js"
    "/bd/src/bd/nls/command.js"
    "/dojo/dijit/nls/common.js"
    "/dojo/dijit/form/nls/ComboBox.js"
    "/dojo/dijit/form/nls/validate.js"
    "/bd/src/bd/widget/root.js"
    "/bd/src/bd/widget/borderContainer.js"
    "/bd/src/bd/dijit/tree.js"
    "/bd/src/bd/widget/statusbar.js"
    "/dojo/dijit/Tree.js"
    "/dojo/dojo/fx.js"
    "/dojo/dojo/DeferredList.js"
    "/dojo/dojo/cookie.js"
    "/dojo/dijit/tree/TreeStoreModel.js"
    "/dojo/dijit/tree/ForestStoreModel.js"
    "/dojo/dojo/fx/Toggler.js"
    "/docder-dev/catalog.js"
    "/bd/src/bd/buildTools.js"))

(defun map-filename (filename)
  (cond
    ((equal (subseq filename 0 8) "/bd/src/")
     (concatenate 'string "/home/rcgill/dev/backdraft/src/" (subseq filename 8)))
    ((equal (subseq filename 0 11) "/dojo/dojo/")
     (concatenate 'string "/home/rcgill/dev/dojo/dojo/" (subseq filename 11)))
    ((equal (subseq filename 0 12) "/dojo/dijit/")
     (concatenate 'string "/home/rcgill/dev/dojo/dijit/" (subseq filename 12)))
    ((equal (subseq filename 0 12) "/docder-dev/")
     (concatenate 'string "/home/rcgill/dev/docder/src/" (subseq filename 12)))))

(defparameter css-url-scanner
  (cl-ppcre:create-scanner "url\\(\\s*('|\")?([^\"\']+)('|\")?\\s*\\)"))

(defparameter css-import-scanner
  (cl-ppcre:create-scanner "^\\s*@import\\s*url"))

(defparameter css-comment-start
  (cl-ppcre:create-scanner "/\\*"))

(defparameter css-comment-end
  (cl-ppcre:create-scanner "\\*/"))

(defparameter css-newline-scanner
  (cl-ppcre:create-scanner "
"))

(defun read-css-source (filename)
  (let ((input (make-array 100000 :element-type 'character))
        (stripped-input ""))
    (with-open-file (in (map-filename filename))
      (let ((length (read-sequence input in)))
        (do ((p 0))
            ((>= p length))
          (let ((start (cl-ppcre:scan css-comment-start input :start p :end length)))
            (if start
                (setf stripped-input (concatenate 'string stripped-input (subseq input p start))
                      p (multiple-value-bind (start end) (cl-ppcre:scan css-comment-end input :start p :end length) end))
                (setf stripped-input (concatenate 'string stripped-input (subseq input p length))
                      p length)
                )))))
    (remove-if (lambda (line) (eq (length line) 0)) (map 'list (lambda (line) (string-trim '(#\Space #\Tab) line)) (cl-ppcre:split css-newline-scanner stripped-input)))))
        

(defun test ()
  (read-css-source "/dojo/dijit/themes/dijit.css")
nil)

(defvar cssid)

(defvar batch)

(defun dump-css(dest root filename)
  (if (not (equal (subseq filename 0 1) "/"))
      (setf filename (concatenate 'string root filename)))
  (let ((path (directory-namestring (pathname filename)))
        (lines (read-css-source filename)))
    ;(format t "travering ~A with root ~A, new root will be ~A~%~A~%" filename root path lines)
    (map nil (lambda (line)
               (multiple-value-bind (start end reg-starts reg-ends) (cl-ppcre:scan css-url-scanner line)
                 (if start
                     (let* ((url-start (aref reg-starts 1))
                            (url-end (aref reg-ends 1))
                            (url (subseq line url-start url-end)))
                       (if (not (eq (aref reg-starts 0) (aref reg-ends 0)))
                           (progn (decf url-start) (incf url-end)))
                       (if (cl-ppcre:scan css-import-scanner line)
                           (dump-css dest path url)
                           (let ((compressed-filename (format nil "_~A.~A" (incf cssid) (pathname-type (pathname url)))))
                             (format dest "~A~A~A~%" (subseq line 0 url-start) compressed-filename (subseq line url-end))
                             (setf batch (cons (format nil "cp ~A ~A" (map-filename (concatenate 'string path url)) compressed-filename) batch)))))
                     (format dest "~A~%" line)))) lines)))

(defun dump-css-docder()
  (setf cssid 0 batch nil)
  (with-open-file (dest "/home/rcgill/dev/docder/src/ccss/ccss.css" :direction :output :if-exists :supersede)
    (dump-css dest "/docder-dev/" "config.css"))
  (with-open-file (dest "/home/rcgill/dev/docder/src/ccss/ccss.sh" :direction :output :if-exists :supersede)
    (dolist (line batch)
      (format dest "~A~%" line))))

(defun dump (dest filename)
  (let ((result (make-array 100000 :element-type 'character)))
    (with-open-file (in filename)
      (let ((length (read-sequence result in)))
        (write-sequence result dest :start 0 :end length)))))

;
; NOTE: must manuall set pause=[] and resume in build.js
;

(defun build-docder ()
  (with-open-file (dest "/home/rcgill/dev/docder/src/build.js" :direction :output :if-exists :supersede)
    (dolist (filename docder-sources)
      (dump dest (map-filename filename)))))

(defparameter cache-files (list
                           "/dojo/dojo/../dijit/templates/MenuItem.html"
                           "/dojo/dojo/../dijit/templates/CheckedMenuItem.html"
                           "/dojo/dojo/../dijit/templates/MenuSeparator.html"
                           "/dojo/dojo/../dijit/templates/Menu.html"
                           "/dojo/dojo/../dijit/templates/MenuBar.html"
                           "/dojo/dojo/../dijit/templates/MenuBarItem.html"
                           "/dojo/dojo/../dijit/form/templates/Button.html"
                           "/dojo/dojo/../dijit/form/templates/DropDownButton.html"
                           "/dojo/dojo/../dijit/form/templates/ComboButton.html"
                           "/dojo/dojo/../dijit/form/templates/TextBox.html"
                           "/dojo/dojo/../dijit/templates/Tooltip.html"
                           "/dojo/dojo/../dijit/form/templates/ValidationTextBox.html"
                           "/dojo/dojo/../dijit/form/templates/ComboBox.html"
                           "/dojo/dojo/../dijit/templates/TreeNode.html"
                           "/dojo/dojo/../dijit/templates/Tree.html"))


(defun dump-cache (dest)
  (dolist (url cache-files)
    (let ((filename (concatenate 'string "/home/rcgill/dev/dojo" (subseq url 13)))
          (text (make-array 100000 :element-type 'character)))
      (with-open-file (in filename)
        (let ((length (read-sequence text in)))
          (format dest "*~A*~A*~A" url length (subseq text 0 length)))))))

(defparameter dijits "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defvar d1)
(defvar d2)
(defvar d3)
(defvar d1c)
(defvar d2c)
(defvar d3c)

(defun inc-d1 ()
  (incf d1)
  (setf d1c (subseq dijits d1 (1+ d1))))

(defun inc-d2 ()
  (if (eq d2 51)
      (and (setf d2 0) (inc-d1))
      (incf d2))
  (setf d2c (subseq dijits d2 (1+ d2))))

(defun inc-id ()
  (if (eq d3 51)
      (and (setf d3 0) (inc-d2))
      (incf d3))
  (setf d3c (subseq dijits d3 (1+ d3)))
  (concatenate 'string d1c d2c d3c))


(defun get-cid (count id) 
  (if (and (equal count 1) (> (length id) 3))
      id
      (inc-id)))

(defparameter id-scanner
  (cl-ppcre:create-scanner "[a-zA-Z][a-zA-Z0-9_]*"))

(defun compress ()
  (setf d1 -1
        d2 -1
        d3 -1
        d1c ""
        d2c ""
        d3c "")
  (let ((input (make-array 1000000 :element-type 'character))
        (dictionary (make-hash-table :test #'equal)))
    (with-open-file (in "/home/rcgill/dev/docder/src/buildc.js")
      (let ((length (read-sequence input in)))
        (do ((p 0))
            ((>= p length))
          (multiple-value-bind (start end) (cl-ppcre:scan id-scanner input :start p :end length)
            (if (not (eq start nil))
                (let ((id (subseq input start end)))
                  (setf (gethash id dictionary) (1+ (gethash id dictionary 0))
                        p end))
                (setf p length))))
        (let ((sorted (make-array (hash-table-count dictionary) :fill-pointer 0)))
          (maphash (lambda (id count) (vector-push (cons count id) sorted)) dictionary)
          (map nil (lambda (item) (setf (gethash (cdr item) dictionary) (get-cid (car item) (cdr item)))) (sort sorted (lambda (lhs rhs) (> (car lhs) (car rhs))))))
        (with-open-file (dest "/home/rcgill/dev/docder/src/buildcc.js" :direction :output :if-exists :supersede)
          (dump-cache dest)
          (maphash (lambda (id cid) (if (<= (length cid) 3) (format dest "~A:~A," cid id))) dictionary)
          (write-line "}" dest)
          (do ((p 0))
              ((>= p length))
            (multiple-value-bind (start end) (cl-ppcre:scan id-scanner input :start p :end length)
              (if (not (eq start nil))
                  (progn
                    (write-sequence (subseq input p start) dest)
                    (write-sequence (gethash (subseq input start end) dictionary) dest)
                    (setf p end))
                  (progn
                    (write-sequence (subseq input p length) dest)
                    (setf p length))))))))))
