;;; Copyright (c) 2015-2016 Donald F Morrison
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

(defpackage :voorhees/doc
  (:use :common-lisp :alexandria :iterate)
  (:export :extract-documentation))

(in-package :voorhees/doc)

(define-constant +includes-directory+ "doc/includes/" :test #'equal)

(defvar *directory*)

(defmacro with-generated-file ((stream-var file-name) &body body)
  `(do-with-generated-file ,file-name #'(lambda (,stream-var) ,@body)))

(defun do-with-generated-file (file-name thunk)
  (when (symbolp file-name)
    (setf file-name (symbol-name file-name)))
  (with-open-file (stream (ensure-directories-exist
                           (make-pathname :name (string-downcase
                                                 (string-trim "*+" file-name))
                                          :type "texi"
                                          :defaults *directory*))
                          :direction :output
                          :if-exists :supersede)
    (format stream "@c *** This is an automatically generated file, do NOT edit it ***~%")
    (funcall thunk stream)))

(defun extract-documentation (&optional (*directory*
                                         (uiop/pathname:subpathname
                                          (asdf:system-source-directory :voorhees)
                                          +includes-directory+)))
  (with-generated-file (s "version")
    (format s "@set VERSION ~A~%" (asdf:component-version (asdf:find-system :voorhees))))
  (multiple-value-bind (package contents)
      (trivial-documentation:package-api :voorhees)
    (when package
      (with-generated-file (s "package")
        (format s "~A~%" package)))
    (iter (for (symbol uses) :on contents :by #'cddr)
          (iter (for properties :in uses)
                (for kind := (getf properties :kind))
                (for documentation := (getf properties :documentation))
                (for command := (cdr (assoc kind '((:function . "deffn")
                                                   (:variable . "defvr")))))
                (when command
                  (with-generated-file (s symbol)
                    (format s "@~A~:[x~;~] ~:(~A~) ~(~A~)~{ ~(~A~)~}~@[~%~A~%@end ~A~]~%"
                            command
                            documentation
                            kind
                            symbol
                            (mapcar #'(lambda (x) (if (listp x) (first x) x))
                                    (getf properties :lambda-list))
                            documentation
                            command)))))))
