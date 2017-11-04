;;; Copyright (c) 2015-2017 Donald F Morrison
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

;; Cribbed verbatim from Roan, with the package name changed to voorhees/doc

(defpackage :voorhees/doc
  (:use :common-lisp :alexandria :iterate)
  (:export :extract-documentation))

(in-package :voorhees/doc)

(define-constant +default-include-file-directory+ "doc/inc/" :test #'string=)

(defvar *include-file-directory*)
(defvar *merged-entries*)

(defgeneric emit-include-file-contents (stream symbol kind
                                        &rest keys &key &allow-other-keys))

(defun include-file-name (symbol kind)
  (let* ((string (symbol-name symbol))
         (star (and (eql (schar string (- (length string) 1)) #\*)
                    (not (eql (schar string 0) #\*)))))
    (format nil "~(~A~:[~;-star~]-~A~)" (string-trim "*+" string) star kind)))

(defun emit-include-file (symbol &rest keys &key kind &allow-other-keys)
  (assert kind)
  (with-open-file (stream (ensure-directories-exist
                           (make-pathname :name (include-file-name symbol kind)
                                          :type "texi"
                                          :defaults *include-file-directory*))
                          :direction :output
                          :if-exists :supersede)
    (format stream "@c *** This is an automatically generated file, do NOT edit it. ***~%~
                      @c ~S ~S~%"
            symbol kind)
    (unless (member kind '(:version :summary))
      (format stream "@anchor{~(~A~:[~;-type~]~)}~%"
              symbol (member kind '(:type :class :structure))))
    (apply #'emit-include-file-contents stream symbol kind keys)))

(defmethod emit-include-file-contents (stream symbol (kind t) &key documentation)
  (declare (ignore symbol kind))
  (format stream "~A~%" documentation))

(defmethod emit-include-file-contents (stream symbol (kind (eql :version)) &key version)
  (declare (ignore symbol kind))
  (format stream "@set VERSION ~A~%" version))

(defun write-defn (stream index command symbol kind documentation &optional lambda-list)
  (when index
    (format stream "@cindex @code{~(~A~)}~%"  symbol))
  (format stream "@~A ~:(~A~) ~(~A~)~{ ~(~A~)~}~%" ;~@[~%~A~]~%@end ~A~%"
          command
          kind
          symbol
          (if (eq kind :macro)
              lambda-list
              (mapcar #'(lambda (x) (if (listp x) (first x) x)) lambda-list)))
  (iter (for (nil sub properties) :in (sort (gethash symbol *merged-entries*)
                                            #'<
                                            :key #'first))
        (when index
          (format stream "@cindex @code{~(~A~)}~%"  sub))
        (format stream "@~Ax ~:(~A~) ~(~A~)~{ ~(~A~)~}~%"
                command
                kind
                sub
                (mapcar #'(lambda (x) (if (listp x) (first x) x))
                        (getf properties :lambda-list))))
  (format stream "~A~%@end ~A~%" documentation command))

(defmacro def-defn-writer (kind command &optional (category kind) has-lambda-list (index t))
  (let ((lambda-list (and has-lambda-list '(lambda-list))))
    `(defmethod emit-include-file-contents (stream symbol (kind (eql ,kind))
                                            &key documentation ,@lambda-list)
       (declare (ignore kind))
       (write-defn stream ,index,command symbol ,category documentation ,@lambda-list))))

(def-defn-writer :package "deftp" :package nil nil)
(def-defn-writer :function "deffn" :function t)
(def-defn-writer :generic-function "deffn" :function t)
(def-defn-writer :macro "deffn" :macro t)
(def-defn-writer :variable "defvr")
(def-defn-writer :constant "defvr")
(def-defn-writer :type "deftp")
(def-defn-writer :structure "deftp" :type)
(def-defn-writer :class "deftp" :type)

(defparameter +summary-scanner+
  (ppcre:create-scanner (format nil "^===summary===~%(.*)~%===endsummary===(?:~%(.*))?$")
                        :single-line-mode t))

(defparameter +merge-scanner+
  (ppcre:create-scanner (format nil "^===merge: ([-*+A-Za-z0-9]+)(?:\\s+(\\d+))?\\s*$")))

(defparameter +lambda-scanner+
  (ppcre:create-scanner (format nil "^===lambda: ([^~%]+)(?:~%(.*))?$")
                        :single-line-mode t))

(defun extract-summary (doc symbol properties)
  (ppcre:register-groups-bind (summary rest) (+summary-scanner+ doc)
    (when summary
      (apply #'emit-include-file symbol
             :kind :summary
             :documentation summary
             properties)
      rest)))

(defun extract-documentation (system-name &key
                                            (package-name system-name)
                                            (directory +default-include-file-directory+))
  (let ((*include-file-directory* (uiop/pathname:subpathname
                                   (asdf:system-source-directory system-name)
                                   directory))
        (*merged-entries* (make-hash-table))
        (skip nil))
    (emit-include-file system-name
                       :kind :version
                       :version (asdf:component-version (asdf:find-system system-name)))
    (multiple-value-bind (package contents)
        (trivial-documentation:package-api package-name)
      (when package
        (when-let ((subdoc (extract-summary package package-name nil)))
          (setf package subdoc))
        (emit-include-file package-name :kind :package :documentation package))
      (iter (for (symbol uses) :on contents :by #'cddr)
            (iter (for properties :in uses)
                  (for doc := (getf properties :documentation))
                  (when-let ((subdoc (extract-summary doc symbol properties)))
                    (setf (getf properties :documentation) (setf doc subdoc)))
                  (ppcre:register-groups-bind (parent order) (+merge-scanner+ doc)
                    (push symbol skip)
                    (push (list (if order (parse-integer order) 0) symbol properties)
                          (gethash (intern (string-upcase parent) package-name)
                                   *merged-entries*))
                    (next-iteration))
                  (ppcre:register-groups-bind (lambda-list rest) (+lambda-scanner+ doc)
                    (setf (getf properties :lambda-list) (read-from-string lambda-list))
                    (setf (getf properties :documentation) rest))))
      (iter (for (symbol uses) :on contents :by #'cddr)
            (unless (member symbol skip)
              (dolist (properties uses)
                (apply #'emit-include-file symbol properties)))))))
