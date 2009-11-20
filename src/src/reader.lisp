(in-package #:js-proc)

;;
;; Convert the source into a vector of strings, one item per line
;;

(defun read-source (stream)
  (let ((text (make-array 100 :element-type 'string :fill-pointer 0 :adjustable t)))
      (do ((line (read-line stream nil 'eof)
                 (read-line stream nil 'eof)))
           ((eql line 'eof))
           (vector-push-extend line text))
    text))
      
(defun read-source-filename (filename)
  (with-open-file (in filename)
    (read-source in)))

(defun read-source-string (string)
  (with-input-from-string (in string)
    (read-source in)))


(defun bulk-read-file (filename)
  (let ((result (make-array 8000 :element-type 'character)))
    (with-open-file (in filename)
      (read-sequence result in))
    result))

;	return ('"' + str.replace(/(["\\])/g, '\\$1') + '"').
;		replace(/[\f]/g, "\\f").replace(/[\b]/g, "\\b").replace(/[\n]/g, "\\n").
;		replace(/[\t]/g, "\\t").replace(/[\r]/g, "\\r"); // string


(defparameter step1
  (cl-ppcre:create-scanner "(\"|\\\\)"))
(defparameter step2
  (cl-ppcre:create-scanner "\\t"))
(defparameter step3
  (cl-ppcre:create-scanner "\\f"))
(defparameter step4
  (cl-ppcre:create-scanner "\\n"))
(defparameter step5
  (cl-ppcre:create-scanner "\\r"))

(defun make-batch (list) 
  (let ((buffer (make-string 100000)))
    (with-open-file (dest "/usr/home/rcgill/dev/dojo/working/bulk.js" :direction :output :if-exists :supersede :element-type 'extended-char)
      (write-string "fileCache= {" dest)
      (let ((first t))
        (dolist (filename list)
          (with-open-file (src (concatenate 'string "/usr/home/rcgill/dev/backdraft" filename) :element-type 'extended-char)
            ;(format t "~A~%" filename)
            (let ((length (read-sequence buffer src)))
              (let ((s (cl-ppcre:regex-replace-all step1 buffer "\\\\\\1" :start 0 :end length)))
                (setf s (cl-ppcre:regex-replace-all step2 s "\\t"))
                (setf s (cl-ppcre:regex-replace-all step3 s "\\f"))
                (setf s (cl-ppcre:regex-replace-all step4 s "\\n"))
                (setf s (cl-ppcre:regex-replace-all step5 s "\\r"))
                (if first
                    (setf first nil)
                    (format dest ","))
                (format dest "~%\"~A\": \"" filename)
                (write-sequence s dest)
                (format dest "\""))))))
      (format dest "~%};"))))


"dojo._base._loader.bootstrap.js"
"dojo._base._loader.loader.js"
"dojo._base._loader.hostenv_browser.js"
"dojo._base.declare"
"dojo._base.lang"
"dojo._base.array"
"dojo._base.connect"
"dojo._base.Deferred"
"dojo._base.json"
"dojo._base.Color"
"dojo._base.browser"
"dojo._base.window"
"dojo._base.event"
"dojo._base.html"
"dojo._base.NodeList"
"dojo._base.query"
"dojo._base.xhr"
"dojo._base.fx"
"docder"
"bd"
"bd.collections"
"bd.lang"
"bd.css"
"dijit._Widget"
"dijit._base"
"dijit._base.focus"
"dijit._base.manager"
"dijit._base.place"
"dojo.AdapterRegistry"
"dijit._base.popup"
"dijit._base.window"
"dijit._base.scroll"
"dijit._base.sniff"
"dijit._base.typematic"
"dijit._base.wai"
"bd.dijit.menu"
"bd.command.item"
"bd.command"
"bd.namespace"
"bd.command.dispatch"
"bd.command.accelerators"
"bd.dijit.mixin.core"
"bd.parentSpace"
"dijit.CheckedMenuItem"
"dijit.MenuItem"
"dijit._Templated"
"dojo.string"
"dojo.parser"
"dojo.date.stamp"
"dojo.cache"
"dijit._Contained"
"dijit.Menu"
"dijit._KeyNavContainer"
"dijit._Container"
"dijit.PopupMenuItem"
"dijit.MenuSeparator"
"dijit.MenuBar"
"dijit.MenuBarItem"
"dijit.PopupMenuBarItem"
"bd.dijit.messagebox"
"dijit.Dialog"
"dojo.dnd.move"
"dojo.dnd.Mover"
"dojo.dnd.common"
"dojo.dnd.autoscroll"
"dojo.dnd.Moveable"
"dojo.dnd.TimedMoveable"
"dojo.fx"
"dojo.fx.Toggler"
"dijit.form._FormMixin"
"dijit._DialogMixin"
"dijit.DialogUnderlay"
"dijit.layout.ContentPane"
"dijit.layout._LayoutWidget"
"dojo.html"
"dojo.i18n"
"dijit.TooltipDialog"
"bd.descriptor.cache"
"bd.dijit.mixin.navigator"
"bd.dijit.tree"
"dijit.Tree"
"dojo.DeferredList"
"dojo.cookie"
"dojo.regexp"
"dijit.tree.TreeStoreModel"
"dijit.tree.ForestStoreModel"
"bd.delayProcManager"
"docder.util"
"docder.docManager"
"dojo.io.script"
"docder.paneManager"
"docder.search"
"dojo.data.ItemFileWriteStore"
"dojo.data.ItemFileReadStore"
"dojo.data.util.filter"
"dojo.data.util.simpleFetch"
"dojo.data.util.sorter"
"bd.dijit.staticText"
"bd.descriptor.processor"
"docder.command"
"bd.resources.commandItems"
"docder.showdown"
"docder.docTypes.javaScriptApiRef"
"bd.dijit.pane"
"bd.dijit.mixin.container"
"bd.test"
"bd.test.result"
"bd.test.publisher"
"bd.test.space"
"bd.test.matchers"
"bd.test.loader"
"bd.test.proc"
"docderTest.unit"
"bd.dijit.root"
"bd.dijit.borderContainer"
"bd.capture"
"bd.dijit.statusbar"
"docder.pageTypes.catalogPage"
    
(defun test1 ()
  (make-batch 
   '(
#|
"./AdapterRegistry.js"
"./back.js"
"./behavior.js"
"./cldr/monetary.js"
"./cldr/supplemental.js"
"./colors.js"
"./cookie.js"
"./currency.js"
"./data/api/Identity.js"
"./data/api/Notification.js"
"./data/api/Read.js"
"./data/api/Request.js"
"./data/api/Write.js"
"./data/ItemFileReadStore.js"
"./data/ItemFileWriteStore.js"
"./data/util/filter.js"
"./data/util/simpleFetch.js"
"./data/util/sorter.js"
"./date/locale.js"
"./date/stamp.js"
"./date.js"
"./DeferredList.js"
"./dnd/autoscroll.js"
"./dnd/Avatar.js"
"./dnd/common.js"
"./dnd/Container.js"
"./dnd/Manager.js"
"./dnd/move.js"
"./dnd/Moveable.js"
"./dnd/Mover.js"
"./dnd/Selector.js"
"./dnd/Source.js"
"./dnd/TimedMoveable.js"
"./fx/easing.js"
"./fx/Toggler.js"
"./fx.js"
"./gears.js"
"./html.js"
"./i18n.js"
"./io/iframe.js"
"./io/script.js"
"./jaxer.js"
"./NodeList-fx.js"
"./NodeList-html.js"
"./number.js"
"./rpc/JsonpService.js"
"./rpc/JsonService.js"
"./rpc/RpcService.js"
"./parser.js"
"./regexp.js"
"./robot.js"
"./robotx.js"
"./string.js"
"./_base/array.js"
"./_base/browser.js"
"./_base/Color.js"
"./_base/connect.js"
"./_base/declare.js"
"./_base/Deferred.js"
"./_base/event.js"
"./_base/fx.js"
"./_base/html.js"
"./_base/json.js"
"./_base/lang.js"
"./_base/NodeList.js"
"./_base/query-sizzle.js"
"./_base/query.js"
"./_base/window.js"
"./_base/xhr.js"
"./_base/_loader/loader_debug.js"
"./_base/_loader/loader_xd.js"
"./_base.js"
"./_firebug/firebug.js"
|#

"/dojo/dojo/./_base/lang.js"
"/dojo/dojo/./_base/declare.js"
"/dojo/dojo/./_base/connect.js"
"/dojo/dojo/./_base/Deferred.js"
"/dojo/dojo/./_base/json.js"
"/dojo/dojo/./_base/array.js"
"/dojo/dojo/./_base/Color.js"
"/dojo/dojo/./_base/browser.js"
"/dojo/dojo/./_base/window.js"
"/dojo/dojo/./_base/event.js"
"/dojo/dojo/./_base/html.js"
"/dojo/dojo/./_base/NodeList.js"
"/dojo/dojo/./_base/query.js"
"/dojo/dojo/./_base/xhr.js"
"/dojo/dojo/./_base/fx.js"
"/dojo/dojo/../dijit/dijit-all.js"
"/dojo/dojo/../dijit/dijit.js"
"/dojo/dojo/../dijit/_base.js"
"/dojo/dojo/../dijit/_base/focus.js"
"/dojo/dojo/../dijit/_base/manager.js"
"/dojo/dojo/../dijit/_base/place.js"
"/dojo/dojo/./AdapterRegistry.js"
"/dojo/dojo/../dijit/_base/popup.js"
"/dojo/dojo/../dijit/_base/window.js"
"/dojo/dojo/../dijit/_base/scroll.js"
"/dojo/dojo/../dijit/_base/sniff.js"
"/dojo/dojo/../dijit/_base/typematic.js"
"/dojo/dojo/../dijit/_base/wai.js"
"/dojo/dojo/./parser.js"
"/dojo/dojo/./date/stamp.js"
"/dojo/dojo/../dijit/_Widget.js"
"/dojo/dojo/../dijit/_Templated.js"
"/dojo/dojo/./string.js"
"/dojo/dojo/../dijit/_Container.js"
"/dojo/dojo/../dijit/layout/_LayoutWidget.js"
"/dojo/dojo/../dijit/_Contained.js"
"/dojo/dojo/../dijit/form/_FormWidget.js"
"/dojo/dojo/../dijit/ColorPalette.js"
"/dojo/dojo/./colors.js"
"/dojo/dojo/./i18n.js"
"/dojo/dojo/./nls/colors.js"
"/dojo/dojo/../dijit/Declaration.js"
"/dojo/dojo/../dijit/Dialog.js"
"/dojo/dojo/./dnd/move.js"
"/dojo/dojo/./dnd/Mover.js"
"/dojo/dojo/./dnd/common.js"
"/dojo/dojo/./dnd/autoscroll.js"
"/dojo/dojo/./dnd/Moveable.js"
"/dojo/dojo/./dnd/TimedMoveable.js"
"/dojo/dojo/./fx.js"
"/dojo/dojo/./fx/Toggler.js"
"/dojo/dojo/../dijit/form/_FormMixin.js"
"/dojo/dojo/../dijit/_DialogMixin.js"
"/dojo/dojo/../dijit/DialogUnderlay.js"
"/dojo/dojo/../dijit/layout/ContentPane.js"
"/dojo/dojo/./html.js"
"/dojo/dojo/../dijit/nls/loading.js"
"/dojo/dojo/../dijit/nls/common.js"
"/dojo/dojo/../dijit/TooltipDialog.js"
"/dojo/dojo/../dijit/Editor.js"
"/dojo/dojo/../dijit/_editor/RichText.js"
"/dojo/dojo/../dijit/_editor/selection.js"
"/dojo/dojo/../dijit/_editor/range.js"
"/dojo/dojo/../dijit/_editor/html.js"
"/dojo/dojo/../dijit/form/nls/Textarea.js"
"/dojo/dojo/../dijit/Toolbar.js"
"/dojo/dojo/../dijit/_KeyNavContainer.js"
"/dojo/dojo/../dijit/ToolbarSeparator.js"
"/dojo/dojo/../dijit/_editor/_Plugin.js"
"/dojo/dojo/../dijit/form/Button.js"
"/dojo/dojo/../dijit/_editor/plugins/EnterKeyHandling.js"
"/dojo/dojo/../dijit/_editor/nls/commands.js"
"/dojo/dojo/../dijit/Menu.js"
"/dojo/dojo/../dijit/MenuItem.js"
"/dojo/dojo/../dijit/PopupMenuItem.js"
"/dojo/dojo/../dijit/CheckedMenuItem.js"
"/dojo/dojo/../dijit/MenuSeparator.js"
"/dojo/dojo/../dijit/MenuBar.js"
"/dojo/dojo/../dijit/MenuBarItem.js"
"/dojo/dojo/../dijit/PopupMenuBarItem.js"
"/dojo/dojo/../dijit/ProgressBar.js"
"/dojo/dojo/./number.js"
;"/dojo/dojo/./cldr/nls/number.js"
"/dojo/dojo/./regexp.js"
"/dojo/dojo/../dijit/TitlePane.js"
"/dojo/dojo/../dijit/Tooltip.js"
"/dojo/dojo/../dijit/Tree.js"
"/dojo/dojo/./cookie.js"
"/dojo/dojo/../dijit/tree/TreeStoreModel.js"
"/dojo/dojo/../dijit/tree/ForestStoreModel.js"
"/dojo/dojo/../dijit/InlineEditBox.js"
"/dojo/dojo/../dijit/form/TextBox.js"
"/dojo/dojo/../dijit/form/CheckBox.js"
"/dojo/dojo/../dijit/form/ComboBox.js"
"/dojo/dojo/../dijit/form/ValidationTextBox.js"
"/dojo/dojo/../dijit/form/nls/validate.js"
"/dojo/dojo/./data/util/simpleFetch.js"
"/dojo/dojo/./data/util/sorter.js"
"/dojo/dojo/./data/util/filter.js"
"/dojo/dojo/../dijit/form/nls/ComboBox.js"
"/dojo/dojo/../dijit/form/CurrencyTextBox.js"
"/dojo/dojo/./currency.js"
;"/dojo/dojo/./cldr/nls/currency.js"
"/dojo/dojo/./cldr/monetary.js"
"/dojo/dojo/../dijit/form/NumberTextBox.js"
"/dojo/dojo/../dijit/form/DateTextBox.js"
"/dojo/dojo/../dijit/_Calendar.js"
"/dojo/dojo/./cldr/supplemental.js"
"/dojo/dojo/./date.js"
"/dojo/dojo/./date/locale.js"
"/dojo/dojo/./cldr/nls/gregorian.js"
"/dojo/dojo/../dijit/form/_DateTimeTextBox.js"
"/dojo/dojo/../dijit/form/FilteringSelect.js"
"/dojo/dojo/../dijit/form/NumberSpinner.js"
"/dojo/dojo/../dijit/form/_Spinner.js"
"/dojo/dojo/../dijit/form/HorizontalSlider.js"
"/dojo/dojo/../dijit/form/VerticalSlider.js"
"/dojo/dojo/../dijit/form/HorizontalRule.js"
"/dojo/dojo/../dijit/form/VerticalRule.js"
"/dojo/dojo/../dijit/form/HorizontalRuleLabels.js"
"/dojo/dojo/../dijit/form/VerticalRuleLabels.js"
"/dojo/dojo/../dijit/form/SimpleTextarea.js"
"/dojo/dojo/../dijit/form/Textarea.js"
"/dojo/dojo/../dijit/layout/AccordionContainer.js"
"/dojo/dojo/../dijit/layout/StackContainer.js"
"/dojo/dojo/../dijit/layout/StackController.js"
"/dojo/dojo/../dijit/form/ToggleButton.js"
"/dojo/dojo/../dijit/layout/AccordionPane.js"
"/dojo/dojo/../dijit/layout/BorderContainer.js"
"/dojo/dojo/../dijit/layout/LayoutContainer.js"
"/dojo/dojo/../dijit/layout/LinkPane.js"
"/dojo/dojo/../dijit/layout/SplitContainer.js"
"/dojo/dojo/../dijit/layout/TabContainer.js"
"/dojo/dojo/../dijit/layout/TabController.js"

)))



#|
/dojo/dojo/../dijit/form/nls/en/validate.js
/dojo/dojo/../dijit/form/nls/en-us/validate.js
/dojo/dojo/../dijit/nls/en/loading.js
/dojo/dojo/../dijit/nls/en-us/loading.js
/dojo/dojo/../dijit/nls/en/common.js
/dojo/dojo/../dijit/nls/en-us/common.js
/dojo/dojo/../dijit/form/nls/en/Textarea.js
/dojo/dojo/../dijit/form/nls/en-us/Textarea.js
/dojo/dojo/../dijit/_editor/nls/en/commands.js
/dojo/dojo/../dijit/_editor/nls/en-us/commands.js
/dojo/dojo/./cldr/nls/en/number.js
/dojo/dojo/./cldr/nls/en-us/number.js
/dojo/dojo/../dijit/form/nls/en/ComboBox.js
/dojo/dojo/../dijit/form/nls/en-us/ComboBox.js
/dojo/dojo/./cldr/nls/en/currency.js
/dojo/dojo/./cldr/nls/en-us/currency.js
/dojo/dojo/./nls/en/colors.js
/dojo/dojo/./nls/en-us/colors.js
/dojo/dojo/./cldr/nls/en/gregorian.js
/dojo/dojo/./cldr/nls/en-us/gregorian.js
|#