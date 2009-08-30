(function() {

f1= function() {
  // Demo a function with no arguments.
};

f2= function(
  a //(integer) documentation for parameter a
){
  // Demo a function with one argument
};

f3= function(
  a, //(integer) documentation for parameter a
  b  //(string) documentation for parameter b
){
  // Demo a function with multiple arguments.
};


f4= function(
  a
){
  // Demo a function with a single undocumented argument.
};

f5= function(
  a,
  b
){
  // Demo a function with a multiple undocumented arguments.
};


f6= function(
  a //(integer) documentation for parameter a
    //          multi-line paragraph
){
  // Demo multi-line parameter documents for a single argument.
};

f7= function(
  a, //(integer) documentation for parameter a
     //          multi-line paragraph
  b  //(integer) documentation for parameter b
     //          multi-line paragraph
){
  // Demo multi-line parameter documents for a multiple arguments.
};


f8= function(
  a //(integer) documentation for parameter a
    //          multi-line paragraph
    //
    //          multi-paragraphs
){
  // Demo multi-line, multi-paragraph parameter documents for a single argument.
};


f9= function(
  a, //(integer) documentation for parameter a
     //          multi-line paragraph
     //
     //          multi-paragraphs
  b  //(integer) documentation for parameter b
     //          multi-line paragraph
     //
     //          multi-paragraphs
){
  // Demo multi-line, multi-paragraph parameter documents for a multiple arguments.
};

f10= function(
  a //(integer) short description for parameter a
    //
    //note here is a note
    //
    //note
    //  and another note
    //
    //note
    //  and a multi-paragraph note
    //
    //  next note paragraph
    ///
    //warn warnings work
    //
    //code code works also
){
  // Demo attributed content in parameter documentation
};

f11= function(
  a //(integer) documentation when a is an integer
    //(string)  documentation when a is a string
){
  // Demo parameter documentation with multiple types
};

f12= function(
  a, //(integer) documentation when a is an integer
     //          another paragraph
     //(string)  documentation when a is a string
     //another paragraph

  b  //(integer)
     //  documentation when b is an integer
     //  multi-paragraph
     //
     //(string)
     //  documentation when b is a string
     //  multi-paragraph
){
  // Demo parameter documentation with multiple types, multi-paragraphs.
};

})();
