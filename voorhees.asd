;;; Copyright (c) 2016-2017 Carnegie Mellon University
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies
;;; or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :asdf-user)

(defsystem :voorhees
  :version (:read-file-form "VERSION")
  :license "MIT"
  :author "Don Morrison <dfm2@cmu.edu>"
  :description "A library to support connecting cognitive models, particularly those
written in ACT-R, to an experimental environment, using JSON"
  :depends-on (:alexandria :iterate :usocket-server :st-json :local-time :cl-ppcre)
  :components ((:file "voorhees"))
  :in-order-to ((test-op (test-op "voorhees/test"))))

(defsystem :voorhees/test
  :license "MIT"
  :author "Don Morrison <dfm2@cmu.edu>"
  :description "Unit tests for Voorhees"
  :depends-on (:voorhees :alexandria :iterate :lisp-unit2 :st-json :bordeaux-threads)
  :components ((:file "tests"))
  :perform (test-op (o s)
             (declare (ignore o s))
             (funcall (find-symbol (symbol-name :test-voorhees) :voorhees/test))))

(defsystem :voorhees/doc
  :license "MIT"
  :author "Don Morrison <dfm2@cmu.edu>"
  :description "Support fo building the documentation for Voorhees"
  :depends-on (:voorhees :alexandria :iterate :trivial-documentation :cl-fad :asdf)
  :components ((:file "extract-documentation")))
