all: voorhees.html voorhees.pdf voorhees.info

voorhees.html: voorhees.texi includes/version-include.texi
	makeinfo --html voorhees.texi
	makeinfo --html --no-split voorhees.texi

voorhees.pdf: voorhees.texi includes/version-include.texi
	makeinfo --pdf voorhees.texi

voorhees.info: voorhees.texi includes/version-include.texi
	makeinfo voorhees.texi

includes/version-include.texi: ../voorhees.lisp ../voorhees.asd ../extract-documentation.lisp
	ccl -Q \
	-e '(ql:quickload :voorhees/doc)' \
	-e '(voorhees/doc:extract-documentation)' \
	-e '(quit)'

clean:
	-rm -r includes *.aux *.fn *.fns *.log *.toc *.vr *.vrs
