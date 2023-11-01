;;; Copyright (c) 2016-2023 Carnegie Mellon University
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

(defpackage :voorhees/test
  (:shadowing-import-from :alexandria :set-equal)
  (:use :common-lisp :voorhees :alexandria :iterate :lisp-unit2)
  (:export :test-voorhees)
  (:import-from :voorhees :unpack-json :pack-json))

(in-package :voorhees/test)

(defun test-voorhees ()
  (with-summary ()
    (run-tests :package :voorhees/test)))

(defpackage :voorhees/test/inner
  (:export :frobozo))

(define-constant +json-data+
    '(("{\"foo\":\"bar\",\"x\":3}" . ((foo . "bar") (x . 3)))
      ("[null,true,false,{},[],\"foo\",{\"foo\":\"foo\"},-17]" .
       #(:null :true :false nil #() "foo" ((foo . "foo")) -17))
      ("{\"symbol\":0,\"SYMBOL\":{},\"sYmboL\":[]}" .
       ((symbol . 0)(|symbol|)(|sYmboL| . #()))))
  :test #'equalp)

(defconstant +tolerance+ 1.0e-5)

(define-test test-unpack-pack ()
  (iter (for (json . lisp) :in +json-data+)
        (let (other (*package* (find-package :voorhees/test)))
          (assert-equalp lisp (setf other (unpack-json (st-json:read-json json))))
          (assert-equalp json (st-json:write-json-to-string (pack-json other)))
          (assert-equalp json (setf other (st-json:write-json-to-string (pack-json lisp))))
          (assert-equalp lisp (unpack-json (st-json:read-json json)))))
  (let ((*package* (find-package :voorhees/test/inner)))
    (assert-eq 'voorhees/test/inner:frobozo
               (car (first (unpack-json (st-json:read-json "{\"frobozo\" : 19}"))))))
  (let ((*json-object-key-package* :voorhees/test/inner))
    (assert-eq 'voorhees/test/inner::|FloopDribble|
               (car (first (unpack-json (st-json:read-json "{\"FloopDribble\" : false}"))))))
  (let ((*json-object-key-package* (find-package "KEYWORD")))
    (assert-eq :|flordbap|
               (car (first (unpack-json (st-json:read-json "{\"FLORDBAP\" : 3.14159}"))))))
  (labels ((fp-test-once (n expected format)
             (let* ((s (st-json:write-json-to-string (pack-json n)))
                    (read (unpack-json (st-json:read-json-from-string s))))
               (assert-equal expected s)
               (assert-true (< (abs (- read n)) +tolerance+))
               (assert-typep format read)))
           (fp-test (n elide no-elide min max prec uc &optional (format *json-float-format*))
             (let ((*json-float-minimum-fixed* (or min *json-float-minimum-fixed*))
                   (*json-float-maximum-fixed* (or max *json-float-maximum-fixed*))
                   (*json-float-precision* (or prec *json-float-precision*))
                   (*json-float-upper-case-exponent* (or uc *json-float-upper-case-exponent*))
                   (*json-float-format* format)
                   (*json-float-elide-trailing-zeros* t))
               (fp-test-once n elide format)
               (setf *json-float-elide-trailing-zeros* nil)
               (fp-test-once n no-elide format))))
    (fp-test 1.0 "1.0" "1.0000" nil nil nil nil)
    (fp-test 20.5 "20.5" "20.50" nil nil 2 nil 'short-float)
    (fp-test -70.3 "-7.03e+1" "-7.0300e+1" nil 5.0 nil nil 'long-float)
    (fp-test 0.0000123 "1.23E-5" "1.2300000E-5" nil nil 7 t 'double-float))
  (let* ((json "{\"x\":1.2,\"y\":[3.4e+10,-5.78e-9]}")
         (*json-object-key-package* :keyword)
         (expr (unpack-json (st-json:read-json-from-string json))))
    (iter (for expected :in '(1.2 3.4e10 -5.78e-9))
          (for observed :in (list* (cdr (assoc :x expr))
                                   (coerce (cdr (assoc :y expr)) 'list)))
          (assert-true (< (abs (- expected observed)) +tolerance+)))
    (assert-equal json (st-json:write-json-to-string
                        (pack-json '((x . 1.2) (y . #(3.4e10 -5.78e-9)))))))
  (assert-error 'error (st-json:write-json-to-string (pack-json #C(1 2))))
  (assert-error 'error (st-json:write-json-to-string (pack-json '(1))))
  (assert-error 'error (st-json:write-json-to-string (pack-json 'symbol))))

(define-test test-describe-json ()
  (labels ((describe-json-to-string (&rest args)
             (with-output-to-string (*standard-output*)
               (let ((*package* (find-package :voorhees/test)))
                 (apply #'describe-json args)))))
    (assert-equal "    json: [ false, { \"key\" : 17 } ]
<=> lisp: #(:FALSE ((KEY . 17)))
"
                  (describe-json-to-string :json "[ false, { \"key\" : 17 } ]"))
    (assert-equal "    json: [true,{\"key\":-8}]
<=> lisp: #(:TRUE ((KEY . -8)))
"
                  (describe-json-to-string :lisp #( :true ((key . -8)))))
    (assert-equal "    json: {}
<=> lisp: NIL

    json: []
<=> lisp: #()

    json: 3
<=> lisp: 3

    json: null
<=> lisp: :NULL
"
                  (describe-json-to-string :json "{}" :lisp #() :lisp 3 :json "null"))))

(defun count-lines-file (file)
  (with-open-file (s file)
    (iter (while (read-line s nil))
          (counting t))))

(defun model-function ()
  (let ((expects (mapcar #'cdr +json-data+))
        (returns (reverse (mapcar #'cdr +json-data+))))
    #'(lambda (x)
        (when (eq x :null)
          (throw :model-done nil))
        (assert-equalp (pop expects) x)
        (pop returns))))

(defun run-other (port server)
  (labels ((run (stream)
             (iter (for send :in (mapcar #'car +json-data+))
                   (for expect :in (reverse (mapcar #'car +json-data+)))
                   (format stream "~A~%" send)
                   (finish-output stream)
                   (assert-equalp expect (string-trim '(#\newline) (read-line stream))))
             (format stream "null~%")
             (finish-output stream)
             (when server
               (return-from run-other))))
    (if server
        (usocket:socket-server nil port #'run)
        (let ((socket (usocket:socket-connect "localhost" port)))
          (unwind-protect (run (usocket:socket-stream socket))
            (usocket:socket-close socket))))))

(define-test test-run-model ()
  (let ((*package* (find-package :voorhees/test))
        (port (+ 8000 (random 10000 (make-random-state t))))
        (log-file (make-pathname :name (format nil "test-log-~D" (random 100000))
                                 :type "log"
                                 :defaults (parse-namestring "temporary-files:"))))
    (format t "~&;; Using port ~D~%" port)
    (labels ((run (client &rest keys &key &allow-other-keys)
               (bt:make-thread (curry #'run-other port client))
               (when client
                 (sleep 2))
               (catch :model-done
                 (apply #'run-model (model-function) port
                        :host (and client "localhost")
                        keys))
               (incf port)))
      ;; TODO test logging, too
      (run t)
      (run nil)
      (run nil :log-file log-file)
      (assert-eql 2 (count-lines-file log-file))
      (run t :log-file (namestring (truename log-file)))
      (assert-eql 4 (count-lines-file log-file))
      (run t :log-file nil)
      (assert-eql 4 (count-lines-file log-file))
      (run t :log-file nil :log-json t)
      (assert-eql 4 (count-lines-file log-file))
      (with-open-file (stream log-file :direction :output :if-exists :supersede)
        (run nil :log-file stream :log-json t))
      (assert-eql 9 (count-lines-file log-file))
      (delete-file log-file))))
