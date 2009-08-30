(function() {

ns1=
  //
  // Demonstrate doc-chunk line triming.
  //
  // Lines are always right-trimmed.
  //
  // A chunk of lines a trimmed by the number of spaces that are found on the line with the
  // minimum number of leading spaces. For example if a chunk has three lines with one, two,
  // and three leading spaces on lines one, two, and three respectively, then one space would
  // be trimmed off each line.
  //
  //note
  // The next code section should be moved to the left by one.
  //code
  //  x
  //   x
  //    x
  //     x
  ///
  //note
  // The next code section has one or more spaces at the end of each line; they should be trimmed
  //code
  // x
  // x
  // x
  ///
  //note
  // The whole point is to be able to put real code in the documentation and remove any common left margin:
  //code
  //     (defun create-object-doc (
  //       ast ;ast for an object literal
  //       raw-doc ;the raw documentation assocated with ast
  //     )
  //       (let ((doc (make-doc :type :variable)))
  //         (do* ((text (sift-pragmas raw-doc #'map-object-pragmas))
  //               (pragma (line-pragma text) (line-pragma text)))
  //             ((not text))
  //           (case pragma
  //             ((:namespace :type :const :enum :variable)
  //              (setf
  //               (doc-type doc) pragma
  //               text (cdr text)))
  //
  //             ((:note :warn :code)
  //              (setf text (get-doc-simple-subsection pragma text (doc-get-ldoc doc))))
  //
  //             ((:todo :todoc :inote)
  //              (setf text (get-doc-simple-subsection pragma text (doc-get-inotes doc))))
  //
  //             (:end
  //              (setf text (cdr text)))
  //
  //             (t
  //              (setf text (get-doc-chunk (or pragma :md) text (doc-get-ldoc doc))))))
  //
  //         (fixup-sdoc doc)
  //
  //         (setf (doc-members doc)
  //               (let ((members (make-hash-table :test 'equal)))
  //                 (dolist (member (second ast) members)
  //                   (let* ((name (first member))
  //                          (ast (third member))
  //                          (raw-doc (or (second member) (get-ast-comment ast))))
  //                     (setf (gethash name members) (create-*-doc ast raw-doc))))))
  //         doc))
  ///
  {};

})();
