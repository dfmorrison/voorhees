Building the Voorhees documentation
===================================

The code in extract-documentation.lisp is used to extract the
docstrings in the Voorhees source to .texi files, which are then
included in the main texinfo documentation, which must then be built
with makeinfo at HTML, PDF, and Info. This is all automated by the
Makefile in the doc/ subdirectory. Just cd to the doc/ subdirectory
and do `make` to run the documentation building process.
