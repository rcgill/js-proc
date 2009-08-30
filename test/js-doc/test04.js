(function() {

ns1=
  //namespace
  //
  //  This is the short doc.
  ///
  //  This is the first paragraph of the long doc section.
  //  It is a multi-line paragraph.
  //
  //  Parts of the long-doc section may be attributed by the note, warn, code, todo, todoc, or inote pragmas.
  //  The contents of these parts consists of one of more paragraphs and may not contain any additional
  //  pragmas (for example, you cannot nest a note).  The contents is always interpretted as markdown.
  //
  //note The demos below are notes; warn, code, todo, todoc, and inote all work the same.
  //
  //note A note that starts on the same line as its pragma is always a single-paragraph note.
  //
  //  Of course a single paragraph note can span multiple lines. (Notice how this paragraph is back
  //  to regular flow.
  //
  //note Here is a single-paragraph note
  //that spans multiple lines.
  //
  //note
  //  Multi-paragraph notes must start on a different line
  //  than the note pragma; in such cases the note is terminated by:
  //
  //    1. The end pragma (/ or end).
  //    2. The end of the comment block.
  //    3. The first blank line if the next pragma encountered (if any) is not the end pragma.
  //
  //  Although a multi-paragraph note must start this way, a single-paragram note can also start this way--
  //  whether a note that starts on a different line that it's pragma is single- or multi-paragraph
  //  is determined by how and where it is terminated.
  ///
  //
  //note
  //  This note is closed by the next blank line because the next pragma is not an end pragma.
  //  Notice it is also a single-paragraph note.
  //
  //  Back to running long doc.
  //
  //note
  //This caused the previous note to close on the first blank line.
  {};

ns2=
  //namespace
  //
  // Demo each ldoc pragma, in a sampling of presentations.
  //
  //note Note 1
  //
  //warn Warn 1
  //
  //code Code 1
  //
  //note
  // Note 2
  //
  //warn
  // Warn 2
  //
  //code
  // Code 2
  //
  //note
  // Note 3, p1
  //
  // Note 3, p2
  ///
  //warn
  // Warn 3, p1
  //
  // Warn 3, p2
  ///
  //code
  // Code 3, p1
  //
  // Code 3, p2
  //
  {};

ns3=
  //namespace
  //
  // Demo each implementation note pragma, in a sampling of presentations.
  //
  //todo Todo 1
  //
  //todoc Todoc 1
  //
  //inote Inote 1
  //
  //todo
  // Todo 2
  //
  //todoc
  // Todoc 2
  //
  //inote
  // Inote 2
  //
  //todo
  // Todo 3, p1
  //
  // Todo 3, p2
  ///
  //todoc
  // Todoc 3, p1
  //
  // Todoc 3, p2
  ///
  //inote
  // Inote 3, p1
  //
  // Inote 3, p2
  //
  {};

})();
