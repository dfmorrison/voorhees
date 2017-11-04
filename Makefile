SOURCES=voorhees.lisp tests.lisp extract-documentation.lisp

all: clean TAGS documentation

documentation: doc/voorhees.pdf doc/voorhees.html

pdf: doc/voorhees.pdf

html: doc/voorhees.html

doc/voorhees.pdf: doc/voorhees.texi doc/inc/voorhees-version.texi
	cd doc; makeinfo --pdf voorhees.texi

doc/voorhees.html: doc/voorhees.texi doc/inc/voorhees-version.texi
	cd doc; makeinfo --html --css-include=voorhees.css --no-split voorhees.texi

doc/inc/voorhees-version.texi: $(SOURCES) voorhees.asd extract-documentation.lisp
	ccl -Q \
	-e '(ql:quickload :voorhees/doc)' \
	-e '(voorhees/doc:extract-documentation :voorhees)' \
	-e '(quit)'

tidy:
	-rm -rf doc/inc doc/voorhees.aux doc/voorhees.fn doc/voorhees.fns doc/voorhees.log doc/voorhees.toc \
	        doc/voorhees.vr doc/voorhees.vrs doc/voorhees.tp doc/voorhees.tps doc/voorhees.cp doc/voorhees.cps

clean: tidy
	-rm -rf doc/voorhees doc/voorhees.pdf doc/voorhees.html

TAGS: $(SOURCES)
	etags $(SOURCES)

touch:
	touch doc/voorhees.texi
