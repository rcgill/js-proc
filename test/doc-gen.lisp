

(defun test-pragma-scanner ()
  (dolist (s
    '("the rest"
      " the rest"
      "// the rest"

      "//pragma"
      "// `pragma"
      "`pragma"
      " `pragma"

      "//pragma the rest"
      "// `pragma the rest"
      "`pragma the rest"
      " `pragma the rest"

      "//()"
      "// `()"
      "`()"
      " `()"

      "//() the rest"
      "// `() the rest"
      "`() the rest"
      " `() the rest"

      "//(type)"
      "// `(type)"
      "`(type)"
      " `(type)"

      "//(type) the rest"
      "// `(type) the rest"
      "`(type) the rest"
      " `(type) the rest"

      "//(type1 | type2)"
      "// `(type1 | type2)"
      "`(type1 | type2)"
      " `(type1 | type2)"

      "//(type1 | type2) the rest"
      "// `(type1 | type2) the rest"
      "`(type1 | type2) the rest"
      " `(type1 | type2) the rest"

      "//pragma(arg1)"
      "// `pragma(arg1)"
      "`pragma(arg1)"
      " `pragma(arg1)"

      "//pragma(arg1) the rest"
      "// `pragma(arg1) the rest"
      "`pragma(arg1) the rest"
      " `pragma(arg1) the rest"

      "//pragma(arg1, arg2)"
      "// `pragma(arg1, arg2)"
      "`pragma(arg1, arg2)"
      " `pragma(arg1, arg2)"

      "//pragma(arg1, arg2) the rest"
      "// `pragma(arg1, arg2) the rest"
      "`pragma(arg1, arg2) the rest"
      " `pragma(arg1, arg2) the rest"))
    (multiple-value-bind (match match-strings) (cl-ppcre:scan-to-strings pragma-scanner s)
      (if (and match (or (aref match-strings 4) (aref match-strings 5)))
          (let ((spaces (make-string (do 
                                      ((sum 0)
                                       (i 2 (incf i)))
                                      ((> i 5)
                                       sum)
                                       (setf sum (+ sum (length (aref match-strings i))))) :initial-element #\space)))
            (format t "~A~%~A~A~A~%[~A(~A)]~%~%" s (or (aref match-strings 1) "") spaces (aref match-strings 6) (aref match-strings 4) (aref match-strings 5)))
          (format t "NO: ~A~%~%" s)))))
    
      