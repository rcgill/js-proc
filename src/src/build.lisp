(in-package #:js-proc)

(defparameter include-scanner
  (cl-ppcre:create-scanner "^\\s*//#include\\s+([^\\s]+)"))

(defun get-include-filename (line) 
  (multiple-value-bind (match filename) (cl-ppcre:scan-to-strings include-scanner line)
    (and match (aref filename 0))))

(defun process-js-src (path inFilename dest)
  (with-open-file (in (concatenate 'string path infilename))
    (format dest "~%~%~%//BEGIN: ~A~%" inFilename)
    (do ((line (read-line in nil 'eof)
               (read-line in nil 'eof)))
        ((eql line 'eof))
      (let ((include-filename (get-include-filename line)))
        (if include-filename
            (process-js-src path include-filename dest)
            (format dest "~A~%" line))))
    (format dest "//END: ~A~%~%~%" inFilename)))


(defun do-build (path inFilename outFilename)
  (with-open-file (dest outFilename :direction :output :if-exists :supersede :element-type 'extended-char)
    (process-js-src path inFilename dest)))

(defun dob ()
  (do-build "/usr/home/rcgill/dev/dojo/dojo-release-1.4.0-src/dojo/bootstrap/" "dojo.js" "/home/rcgill/dev/dojo/dojo-release-1.4.0-src/dojo/dojo.js"))

(defun investigate-old-dojo-resource (in) 
  (let ((src (read-source in))
        (last-loader-statement 0))
    (do ((i 0 (1+ i))
         (end (length src)))
        ((>= i end))
;;      (format t "~A~%" (aref src i))
      (if (cl-ppcre:scan "dojo\\.require" (aref src i))
          (setf last-loader-statement i)))
    (do ((i 0 (1+ i)))
        ((> i last-loader-statement))
      (format t "~A~%" (aref src i)))))
       
(defun convertClasses(class-names)
  (dolist (class-name class-names)
    (format t "~A:~%" class-name)
    (with-open-file (in (concatenate 'string 
                                     "/usr/home/rcgill/dev/dojo/dojo-release-1.4.0-src/"
                                     (cl-ppcre:regex-replace-all "\\." class-name "/")
                                     ".js"))
      (investigate-old-dojo-resource in))
    (format t "~%~%~%~%~%")))


(defun doconvert()
  (convertClasses classModules))

(defun make-dijit-filename (filename)
  (concatenate 'string "/usr/home/rcgill/dev/dojo/dojo-release-1.4.0-src/dijit/" filename ".js"))

(defun make-dijit-out-filename (filename)
  (concatenate 'string "/usr/home/rcgill/dev/dojo/dojo-release-1.4.0-src/dijit/" filename ".js"))

(defparameter dijit-sources 
  '("_editor/plugins/EnterKeyHandling"
    "_editor/plugins/TabIndent"
    "_editor/plugins/ViewSource"
    "_editor/plugins/Print"
    "_editor/plugins/LinkDialog"
    "_editor/plugins/ToggleDir"
    "_editor/plugins/FontChoice"
    "_editor/plugins/FullScreen"
    "_editor/plugins/TextColor"
    "_editor/plugins/NewPage"
    "_editor/plugins/AlwaysShowToolbar"
    "_editor/RichText"
    "_editor/range"
    "_editor/html"
    "_editor/_Plugin"
    "_editor/selection"
    "tree/ForestStoreModel"
    "tree/_dndSelector"
    "tree/_dndContainer"
    "tree/dndSource"
    "tree/TreeStoreModel"
    "_base/typematic"
    "_base/manager"
    "_base/scroll"
    "_base/focus"
    "_base/popup"
    "_base/wai"
    "_base/window"
    "_base/place"
    "layout/_TabContainerBase"
    "layout/StackContainer"
    "layout/StackController"
    "layout/TabContainer"
    "layout/TabController"
    "layout/ContentPane"
    "layout/SplitContainer"
    "layout/BorderContainer"
    "layout/LayoutContainer"
    "layout/_LayoutWidget"
    "layout/LinkPane"
    "layout/ScrollingTabController"
    "layout/AccordionContainer"
    "layout/AccordionPane"
    "_tree/dndSource"
    "form/CurrencyTextBox"
    "form/ComboBox"
    "form/NumberSpinner"
    "form/TimeTextBox"
    "form/Textarea"
    "form/_Spinner"
    "form/HorizontalRuleLabels"
    "form/VerticalRuleLabels"
    "form/DateTextBox"
    "form/TextBox"
    "form/Form"
    "form/_FormSelectWidget"
    "form/_FormMixin"
    "form/_DateTimeTextBox"
    "form/Button"
    "form/ToggleButton"
    "form/Select"
    "form/HorizontalRule"
    "form/Slider"
    "form/FilteringSelect"
    "form/MappedTextBox"
    "form/CheckBox"
    "form/DropDownButton"
    "form/RadioButton"
    "form/MultiSelect"
    "form/ComboButton"
    "form/HorizontalSlider"
    "form/RangeBoundTextBox"
    "form/NumberTextBox"
    "form/ValidationTextBox"
    "form/VerticalRule"
    "form/_FormWidget"
    "form/VerticalSlider"
    "form/SimpleTextarea"
    "_Contained"
    "Menu"
    "dijit"
    "MenuBar"
    "Tooltip"
    "ToolbarSeparator"
    "CheckedMenuItem"
    "Dialog"
    "_DialogMixin"
    "Tree"
    "Calendar"
    "_Calendar"
    "_base"
    "TooltipDialog"
    "robotx"
    "_Templated"
    "_Widget"
    "_HasDropDown"
    "TitlePane"
    "Declaration"
    "Editor"
    "ColorPalette"
    "ProgressBar"
    "_TimePicker"
    "_KeyNavContainer"
    "InlineEditBox"
    "Toolbar"
    "MenuSeparator"
    "MenuBarItem"
    "_Container"
    "DialogUnderlay"
    "MenuItem"
    "PopupMenuBarItem"
    "PopupMenuItem"
    "_base/sniff"
    "dijit-all"))

;;BY HAND..../robot.js


(defparameter require-provide-scanner
  (cl-ppcre:create-scanner "^\\s*dojo\.(require|provide)\\s*\\(\\s*\"([^\"]+)\"\\s*\\)"))

(defparameter require-localization-scanner
  (cl-ppcre:create-scanner "^\\s*dojo\.requireLocalization\\s*\\(\\s*\"([^\"]+)\"\\s*\\,\\s*\"([^\"]+)\"\\s*\\)"))


(defun require-name-to-module-name (name)
  (concatenate 'string "\"" (cl-ppcre:regex-replace-all "\\." name "/") "\""))

(defun cat-array (elements)
  (let ((length (length elements)))
    (cond
      ((= length 0) 
       "[]")
      
      ((= length 1) 
       (concatenate 'string "[" (aref elements 0) "]"))
      
      (t 
       (let ((result (aref elements 0)))
         (do ((i 1 (1+ i)))
             ((eq i length))
           (setf result (concatenate 'string result ", " (aref elements i))))
         (concatenate 'string "[" result "]"))))))

(defun convert-dijit()
  (dolist (filename dijit-sources)
    (let ((contents (read-source-filename (make-dijit-filename filename)))
          (filtered-contents (make-array 100 :element-type 'string :fill-pointer 0 :adjustable t))
          (requires (make-array 2 :initial-contents '("dojo" "dijit") :element-type 'string :fill-pointer 2 :adjustable t))
          (provides nil))
      (map nil (lambda (line)
                 (multiple-value-bind (match strings) (cl-ppcre:scan-to-strings require-provide-scanner line)
                   (if match
                       (cond
                         ((equal "require" (aref strings 0))
                          (vector-push-extend (aref strings 1) requires))
                         ((equal "provide" (aref strings 0))
                          (if provides
                              (format t "~%ERROR(~A): multiple provides~%" filename))
                          (setf provides (aref strings 1))))
                       (multiple-value-bind (match strings) (cl-ppcre:scan-to-strings require-localization-scanner line)
                         (if match
                             (vector-push-extend (concatenate 'string 
                                                              "i18n!" 
                                                              (aref strings 0)
                                                              ".nls." 
                                                              (aref strings 1)) requires)
                             (vector-push-extend line filtered-contents))))))
           contents)
      (setf requires (map 'vector #'require-name-to-module-name requires))
      (with-open-file (dest (make-dijit-out-filename filename) :direction :output :if-exists :supersede :element-type 'extended-char)
        (format dest "dojo.def(~A, ~A, function(dojo, dijit) {~%" (require-name-to-module-name provides) (cat-array requires))
        (map nil (lambda (line) (format dest "~A~%" line)) filtered-contents)
        (format dest "});~%")))))

    

(defparameter modifierModules '(
  "dijit._base"
  "dijit._base.place"
  "dijit._base.sniff"
  "dijit._base.window"
  "dijit._base.manager"
  "dijit._base.scroll"
  "dijit._base.focus"
  "dijit._base.typematic"
  "dijit._base.wai"
  "dijit._base.popup"

  "dijit.dijit"
  "dijit.dijit-all"

  "dijit._editor.html"
  "dijit._editor.selection"
  "dijit._editor.range"
  "dijit._Calendar"
  "dijit.robot"
  "dijit.robotx"

  "dijit._tree.dndSource"
  "dijit.form.Slider"))

(defparameter classModules '(
  "dijit.MenuBarItem"
  "dijit._editor.plugins.AlwaysShowToolbar"
  "dijit._editor.plugins.NewPage"
  "dijit._editor.plugins.TabIndent"
  "dijit._editor.plugins.EnterKeyHandling"
  "dijit._editor.plugins.ViewSource"
  "dijit._editor.plugins.Print"
  "dijit._editor.plugins.LinkDialog"
  "dijit._editor.plugins.ToggleDir"
  "dijit._editor.plugins.FontChoice"
  "dijit._editor.plugins.FullScreen"
  "dijit._editor.plugins.TextColor"
  "dijit._editor.RichText"
  "dijit._editor._Plugin"
  "dijit._Container"
  "dijit.MenuItem"
  "dijit.tree._dndSelector"
  "dijit.tree.dndSource"
  "dijit.tree._dndContainer"
  "dijit.tree.TreeStoreModel"
  "dijit.tree.ForestStoreModel"
  "dijit.MenuSeparator"
  "dijit.MenuBar"
  "dijit.Tooltip"
  "dijit.CheckedMenuItem"
  "dijit._Contained"
  "dijit.PopupMenuBarItem"
  ;;"dijit.Dialog"
  "dijit.layout.AccordionContainer"
  "dijit.layout.AccordionPane"
  "dijit.layout.ScrollingTabController"
  ;;"dijit.layout.StackContainer"
  "dijit.layout.StackController"
  "dijit.layout.TabContainer"
  "dijit.layout.TabController"
  "dijit.layout.ContentPane"
  "dijit.layout.SplitContainer"
  ;;"dijit.layout.BorderContainer"
  "dijit.layout.LayoutContainer"
  "dijit.layout._LayoutWidget"
  "dijit.layout.LinkPane"
  "dijit.layout._TabContainerBase"
  "dijit._DialogMixin"
  "dijit.DialogUnderlay"
  ;;"dijit.Menu"
  "dijit.PopupMenuItem"
  "dijit.Calendar"
  "dijit.TooltipDialog"
  "dijit._Templated"
  "dijit._Widget"
  "dijit.ToolbarSeparator"
  "dijit.TitlePane"
  "dijit.Declaration"
  "dijit.Editor"
  "dijit._HasDropDown"
  "dijit.form.NumberTextBox"

  "dijit.form._FormWidget"
  "dijit.form.ValidationTextBox"
  "dijit.form.ComboBox"
  "dijit.form.CurrencyTextBox"
  "dijit.form.TimeTextBox"
  "dijit.form.Textarea"
  "dijit.form._Spinner"
  "dijit.form.VerticalRuleLabels"
  "dijit.form.NumberSpinner"
  "dijit.form.DateTextBox"
  "dijit.form.TextBox"
  "dijit.form.VerticalRule"
  "dijit.form.Form"
  "dijit.form.HorizontalRuleLabels"
  "dijit.form._FormSelectWidget"
  "dijit.form._FormMixin"
  "dijit.form._DateTimeTextBox"
  "dijit.form.Button"
  "dijit.form.VerticalSlider"
  "dijit.form.ToggleButton"
  "dijit.form.Select"
  "dijit.form.HorizontalRule"
  "dijit.form.FilteringSelect"
  "dijit.form.MappedTextBox"
  "dijit.form.CheckBox"
  "dijit.form.SimpleTextarea"
  "dijit.form.DropDownButton"
  "dijit.form.RadioButton"
  "dijit.form.MultiSelect"
  "dijit.form.ComboButton"
  "dijit.form.HorizontalSlider"
  "dijit.form.RangeBoundTextBox"
  "dijit.ProgressBar"
  "dijit._TimePicker"
  "dijit.ColorPalette"
  "dijit.InlineEditBox"
  "dijit._KeyNavContainer"
  ;;"dijit.Tree"
  ;;"dijit.Toolbar"
                             ))
