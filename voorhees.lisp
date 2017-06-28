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

(defpackage :voorhees
  (:nicknames :vh)
  (:use :common-lisp :alexandria :iterate :split-sequence)
  (:export :*actr-package*
           :*json-float-elide-trailing-zeros*
           :*json-float-format*
           :*json-float-maximum-fixed*
           :*json-float-minimum-fixed*
           :*json-object-key-package*
           :*json-float-precision*
           :*json-float-upper-case-exponent*
           :chunkify
           :describe-json
           :run-model
           :stapify))

(in-package :voorhees)



(defparameter *json-object-key-package* nil
  "A package designator. When decoding JSON to a Lisp expression the keys of any JSON
objects will be converted to symbols interned in this package. If @code{nil}, the default,
the current package is used.")

(defparameter *json-float-format* 'single-float
  "A type specifier for a subtype of @code{float}. When floating point numbers appear
in JSON that is being converted to Lisp, they will be converted to instances of this
type. The default is @code{single-float}.")

(defparameter *json-float-minimum-fixed* 1.0e-3
  "@include includes/json-float-maximum-fixed.texi
A positive, real number. Determines whether floating point numbers, when converted to
JSON, are represented in fixed or exponential format. If the absolute value of the number
is greater than or equal to @code{*json-float-minimum-fixed*} (default value 1.0e-3) and
less than @code{*json-float-maximum-fixed*} (default value 1.0e+7) fixed format will be
used, and otherwise exponential format. It is strongly recommended that
@code{*json-float-minimum-fixed*} and @code{*json-float-precision*} only be assigned
values such that the former's first non-zero digit will always appear in the printed
representation.")

(defparameter *json-float-maximum-fixed* 1.0e+7)

(defparameter *json-float-precision* 4
  "A positive integer, the maximum number of digits to be displayed after the decimal
point when converting floating point numbers to JSON. If
@code{*json-float-elide-trailing-zeros*} is false exactly this many digits will be
displayed after the decimal point. The default value is 4.")

(defparameter *json-float-elide-trailing-zeros* t
  "A generalized boolean. If true, the default then when converting floating point numbers
to JSON the last digit following the decimal point, but before the exponentiation marker,
if any, will be non-zero, even if this means fewer than @code{*json-float-precision*}
digits will be shown; an exception is made for the case where all digits following the
decimal point would be zero, in which case one will be shown. If
@code{*json-float-elide-trailing-zeros*} is false exactly @code{*json-float-precision*}
digits will always be show after the decimal point.")

(defparameter *json-float-upper-case-exponent* nil
  "A generalized boolean. If true then when converting floating point numbers to JSON, if
they are displayed in exponential notation an upper case `E' will be used; and otherwise a
lower case `e' is used. The default value is @code{nil}.")



(defun run-model (model-fn port &key host timeout log-file log-json repeat)
  "@table @var
@item model-fn
a designator for a function of one argument

@item port
a postive integer

@item host
a string, positive integer, vector of non-negative integers or @code{nil}

@item timeout
a positive integer or @code{nil}

@item log-file
a pathname, string, stream or @code{nil}

@item log-json
a generalized boolean

@item repeat
@code{nil}, @code{t} or a function
@end table

Creates a TCP socket, as a client or a server, and then repeatedly reads JSON values from
it, calls @var{model-fn} on the value read, and, if @var{model-fn} returns a
non-@code{nil} value, writes the corresonding JSON value to the socket.

The @var{model-fn} should be a function of one argument, which runs the underlying
congnitive model. When called @var{model-fn} will be passed the Lisp representation
of the JSON value that was read. If @var{model-fn} returns @code{nil} no response is
written to the socket. Otherwise the value returned by @var{model-fn} should be the
Lisp representation of a JSON value, which will be written to the socket. Note that
this means is it not possible to return a JSON empty object, @code{@{@}}, though it is
possible to return a JSON empty array, @code{[]}, or JSON @code{null}.

The @var{port} is the network port to use. It should typically be in the range 1,024
to 49,151, inclusive, to avoid conflicting with well known or ephemeral ports.

If @var{host} is @code{nil}, the socket will be opened passively, acting as a server
waiting for a connection. Otherwise @var{host} should be a name or IP address, and the
socket will be opened actively, attempting to connect to that host. The @var{host}
may be
@itemize
@item
a string representing a network name, such as @code{\"halle.psy.cmu.edu\"} or
@code{\"localhost\"}

@item
a string representation of an IP address, such as @code{\"128.2.116.159\"} or
@code{\"127.0.0.1\"}

@item
a single integer, comprised of the four bytes of the usual notation;
for example, @code{2147644575} is the same as @code{\"128.2.116.159\"}

@item
a vector of four integers, such as @code{#(128 2 116 159)} or @code{#(127 0 0 1)}
@end itemize
@noindent
Voorhees does @emph{not} support IPv6, only IPv4.

The @var{timeout} argument is only used if @var{host} is non-@code{nil}, and is the
time @code{run-model} will wait for a connection to be established before signalling
an error that the connection could not be made. If @var{timeout} is @code{nil}, the
default, it will wait indefinitely.

If @var{log-file} is non-@code{nil} it is the destination to which logging information
should be written. If it is @code{nil}, the default, no log is written. If it is a
stream, the log is simply written to it. If it is a pathame or string, it denotes a
file. If the file exists it is appended to, not overwritten. If it does not yet exist
it is created when the first log record is written.

If @var{log-json} is true the log will contain records for every JSON value read or
written, in addition to records for the opening and closing of connections. If it is
false, the default, only connection information will be recorded.

TODO document @var{repeat}.

The application on the other end of the TCP connection should follow every JSON value
it sends with a newline; the values may also have newlines within them, if desired.
When sending responses, @code{run-model} will always append a new line, and will never
embed one within the JSON value it sends.

Concurrent connections are not supported by @code{run-model}.
That said, if a passive connection is opened it will not
refuse connections after the first, they will mere sit idly until the first connection
is closed, at which point another will have its opportunity to run the model.

If @code{run-model} opens an active connection, that is if @var{host} is
non-@code{nil}, when the connection is closed @code{run-model} will return. However, if
it opens a passive connection @code{run-model} will never return, but will wait for
connections indefintely. Use Control-C to kill it if necessary, or simply terminate Lisp."
  (check-type port (integer 1) "a positive integer")
  (check-type host (or string (integer 1) vector null)
              "a string, positive integer, vector of non-negative integers or NIL")
  (check-type timeout (or (integer 0) null) "a positive integer or NIL")
  (check-type log-file (or pathname string stream null) "a pathname, string, stream or NIL")
  (check-type repeat (or (member nil t) function))
  (when (and log-file (not (streamp log-file)))
    (setf log-file (merge-pathnames log-file)))
  (if host
      (let ((socket (usocket:socket-connect host port :timeout timeout)))
        (unwind-protect
             (%run-model (usocket:socket-stream socket)
                         model-fn
                         log-file
                         log-json
                         (usocket:get-peer-address socket))
          (usocket:socket-close socket)))
      (block nil
        (usocket:socket-server nil
                               port
                               #'(lambda (&rest args)
                                   (let ((result (apply #'%run-model args)))
                                     (when (and repeat
                                                (or (not (functionp repeat))
                                                    (funcall repeat)))
                                         (return result))))
                               (list model-fn log-file log-json nil)))))

(defun write-log (log-path-or-stream format-string &rest args)
  (let ((*print-length* 10) (*print-level* 4) (*print-pretty* nil))
    (labels ((write-log-entry (stream)
               (local-time:format-timestring stream (local-time:now)
                                             :format local-time:+rfc-1123-format+)
               (format stream " ~?~%" format-string args)))
      (etypecase log-path-or-stream
        (null)
        (stream (write-log-entry log-path-or-stream))
        (pathname (with-open-file (stream log-path-or-stream
                                          :direction :output
                                          :if-exists :append
                                          :if-does-not-exist :create)
                    (write-log-entry stream)))))))

;; TODO Some clever error handling would be useful here; right now things just go South
;;      if bad JSON is presented, etc.
(defun %run-model (stream model-fn log-file log-json remote-host)
  (write-log log-file "run-model opened a connection ~:[from~;to~] ~{~D~^.~}"
             remote-host (coerce (or remote-host usocket:*remote-host*) 'list))
  (unwind-protect
       (loop for (json success) = (multiple-value-list (read-multiline-json stream))
             for unpacked-json = (and success (unpack-json json))
             while success
             when log-json do (write-log log-file "received: ~S" unpacked-json)
             do (when-let ((response (funcall model-fn unpacked-json)))
                  (when log-json
                    (write-log log-file "sending: ~S" response))
                  (st-json:write-json (pack-json response) stream)
                  (terpri stream)
                  (finish-output stream)))
    (write-log log-file "run-model done")))



(define-constant +whitespace+ '(#\space #\newline #\tab #\return #\page) :test #'equal)

(defmethod st-json:read-json :around ((in stream) &optional (junk-allowed-p t))
  (declare (ignore in junk-allowed-p))
  (let ((*read-default-float-format* *json-float-format*))
    (call-next-method)))

;; Unfortunately st-json has a bug where it is unable to recognize the end of a JSON
;; object without reading a whole lot more first, making it rather difficult to use in
;; a streaming context. So we limit ourselves to JSON objects followed by a new line.
;; TODO Investigate further alternatives to st-json (cl-json is worse; yason has its own
;;      problems, but may be able to be made to work in this context).
(defun read-multiline-json (stream)
  (loop with lines = (make-array 100 :element-type 'character
                                 :adjustable t :fill-pointer 0)
        for (line missing-newline) = (multiple-value-list (read-line stream nil))
        while line
        do (format lines "~A~:[~%~;~]" line missing-newline)
        do (handler-case
               (return (values (st-json:read-json-from-string lines) t))
             (st-json:json-eof-error ()))
        finally (if (zerop (length (string-trim +whitespace+ lines)))
                    (return (values nil nil))
                    (error "End of file encountered while parsing multi-line JSON ~A"
                           lines))))

(defun unpack-json (json)
  (etypecase json
    (list (coerce (mapcar #'unpack-json json) 'vector))
    (st-json:jso (iter (for (key . value) :in (slot-value json 'st-json::alist))
                       (for sym := (string-to-symbol key))
                       (when (and (member sym '(:@id :@value))
                                  (not (typep value '(or string (integer 0 *)))))
                         (error "The value of an :@id (~S) must be a string or a non-negative integer."
                                value))
                       (collect (cons sym (unpack-json value)))))
    ((or keyword number string) json)))

;; TODO Figure out appropriate error handling here. Since, unlike unpack-json, this
;;      function is being passed user supplied data, it may very well be ill-formed, and
;;      that probably shouldn't take everything down with it.
(defun pack-json (json)
  (etypecase json
    (list (apply #'st-json:jso (loop for (car . cdr) in json
                                     nconc (list (symbol-to-string car)
                                                 (pack-json cdr)))))
    ((or keyword number string) json)
    ;; the vector clause must come after the string clause, since is string is a vector
    (vector (mapcar #'pack-json (coerce json 'list)))))

(defun string-to-symbol (s)
  (intern (maybe-flip-string-case s)
          (if (and (> (length s) 0) (eql (char s 0) #\@))
              :keyword
              (or *json-object-key-package* *package*))))

(defun symbol-to-string (s)
  (maybe-flip-string-case (symbol-name s)))

(defun maybe-flip-string-case (s)
  (loop with lower-case-seen = nil
        with upper-case-seen = nil
        for c across s
        until (and lower-case-seen upper-case-seen)
        when (lower-case-p c) do (setf lower-case-seen t)
        when (upper-case-p c) do (setf upper-case-seen t)
        finally (return (cond ((not (xor lower-case-seen upper-case-seen)) s)
                              (upper-case-seen (string-downcase s))
                              (t (string-upcase s))))))

(defparameter +float-scanner+
  (ppcre:create-scanner "^(-?\\d+\\.\\d\\d*?)0*((?:e[+-]?\\d+)?)?$"
                        :case-insensitive-mode t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when-let ((method (find-method #'st-json:write-json-element ()
                                  (mapcar #'find-class '(real t))
                                  nil)))
    ;; stiffle the warning about the subsequent method redefinition
    (remove-method #'st-json:write-json-element method)))

(defmethod st-json:write-json-element ((element real) stream)
  (let ((s (if (and *json-float-minimum-fixed*
                    *json-float-maximum-fixed*
                    (>= (abs element) *json-float-minimum-fixed*)
                    (< (abs element) *json-float-maximum-fixed*))
               (format nil "~,vF" *json-float-precision* element)
               (format nil "~,v,,,,,vE"
                       *json-float-precision*
                       (if *json-float-upper-case-exponent* #\E #\e)
                       element))))
    (if *json-float-elide-trailing-zeros*
        (ppcre:register-groups-bind (pre post) (+float-scanner+ s)
          (princ pre stream)
          (princ post stream))
        (princ s stream))))

;; TODO Deal more gracefully with errors in the following.
(defun describe-json (&rest keys &key json lisp)
  "@table @var
@item json
a string

@item lisp
an expression
@end table

Displays the correspondance between a JSON value and a Lisp expression.
Supply the JSON value as a string value of @var{json} or the Lisp expression as the
value of @var{lisp}. The result is printed to @code{*standard-output*}. Multiple
occurances of @var{json} or @var{lisp} may be provided, in which case the
correspondances for all provided values will be shown.

Examples:
@example
@group
 * (vh:describe-json
     :json \"@{\\\"key\\\": 17@}\")

     json: @{\"key\": 17@}
 <=> lisp: ((KEY . 17))
@end group
@end example

@example
@group
 * (vh:describe-json
     :lisp #(1 \"a string\" () :null))

     json: [1,\"a string\",@{@},null]
 <=> lisp: #(1 \"a string\" NIL :NULL)
@end group
@end example

@example
@group
 * (vh:describe-json
     :json \"[1,@{\\\"s\\\":\\\"String\\\",\\\"NUM\\\":17,\\\"Array\\\":[2,7]@}, 3]\")

     json: [1,@{\"s\":\"String\",\"NUM\":17,\"Array\":[2,7]@}, 3]
 <=> lisp: #(1 ((S . \"String\") (|num| . 17) (|Array| . #(2 7))) 3)
@end group
@end example

@example
@group
 * (vh:describe-json
     :json \"@{\\\"aa*\\\":\\\"aa*\\\", \\\"AA*\\\":\\\"AA*\\\",\\\"Aa*\\\":\\\"Aa*\\\"@}\"
     :lisp nil
     :json \"[true, false, null, @{@}, [], \\\"\\\"]\")

     json: @{\"aa*\":\"aa*\", \"AA*\":\"AA*\",\"Aa*\":\"Aa*\"@}
 <=> lisp: ((AA* . \"aa*\") (|aa*| . \"AA*\") (|Aa*| . \"Aa*\"))

     json: @{@}
 <=> lisp: NIL

     json: [true, false, null, @{@}, [], \"\"]
 <=> lisp: #(:TRUE :FALSE :NULL NIL #() \"\")
@end group
@end example"
  (declare (ignore json lisp))
  (labels ((format-list (label list)
             (format t "~&~10<~A: ~>~A~%~{~10T~A~%~}" label (first list) (rest list)))
           (describe-one (json lisp)
             (format-list "json" (split-sequence #\newline (string-trim +whitespace+ json)))
             (format-list "<=> lisp" (split-sequence #\newline (format nil "~:W" lisp)))))
    (loop for (key value . rest) on keys by #'cddr
          do (ecase key
               (:json (check-type value string)
                      (describe-one value
                                    (unpack-json (st-json:read-json-from-string value))))
               (:lisp (describe-one (st-json:write-json-to-string (pack-json value))
                                    value)))
          when rest do (terpri *standard-output*)))
  (values))



;; ACT-R chunk creation

;; ACT-R has a hate-hate relationship with packages: we don't know at compile time what
;; package ACT-R's public API will be in, and even whichever one it is in at run time it's
;; all internal symbols. So we create indirect versions of all the functions we need in
;; the Voorhees package that simply redirect to whatever package ACT-R is loaded into at
;; run time.

(defun guess-actr-package ()
  (and (member :ACT-R *features*)
       (iter (for name :in '(#:define-buffer-fct
                             #:schedule-mod-buffer-chunk
                             #:pprint-chunks-fct))
             (reducing (mapcar #'symbol-package (apropos-list name))
                       :by #'intersection
                       :into result)
             (finally (return (cond ((and result (null (rest result))) (first result))
                                    (result nil)
                                    (t (find-package '#:common-lisp-user))))))))

(defvar *actr-package* (guess-actr-package)
  "Should be set to a package designator for the package into which ACT-R was loaded.
Voorhees tries to guess a suitable default, which will usually be correct. If it fails to
come up with a unique guess, or if ACT-R has not yet been loaded, it will be set to
@code{:common-lisp-user}.")

(defmacro define-actr-function (name)
  (check-type name symbol)
  `(defun ,name (&rest args)
    (apply #'uiop:symbol-call *actr-package* ,(symbol-name name) args)))

(define-actr-function add-dm-fct)
(define-actr-function current-model)
(define-actr-function define-chunks-fct)
(define-actr-function extend-possible-slots)
(define-actr-function merge-dm-fct)
(define-actr-function overwrite-buffer-chunk)
(define-actr-function purge-chunk-fct)
(define-actr-function set-buffer-chunk)

(defparameter *act-r-7-confirmed* nil)

(defun ensure-act-r-7 ()
  (unless *act-r-7-confirmed*
    (unless (member :act-r-7 *features*)
      (unless (member :act-r *features*)
        (error "ACT-R does not appear to be loaded."))
      (error "Voorhees requires ACT-R version 7")) ; what do we do when there's ACT-R 8?
    (setf *act-r-7-confirmed* t))
  (unless (current-model)
    (error "There is no current ACT-R model")))

(define-constant +non-alphanumeric-slot-prefix+ "S" :test #'string=)

(defvar *chunk-table*)

(defun chunkify (json &key buffer overwrite (merge t))
  "@table @var
@item json
an a-list

@item buffer
a non-nil symbol, naming an ACT-R buffer

@item overwrite
a generalized boolean

@item merge
a generalize boolean
@end table
Creates an ACT-R chunk from a JSON object. The @var{json} should be an a-list
representing a JSON object. If @var{buffer} is a non-nil symbol, it is the name of an
ACT-R buffer into which to place the resulting chunk; if @var{buffer} is not supplied or
is @code{nil} the chunk is placed in declarative memory. The @var{overwrite} argument
is only used if the chunk is being placed into a buffer; if true, it says to not clear
the current contents of the buffer to declarative memory. The default if not supplied
is false; that is, by default, the buffer is cleared to declarative memory before the new
chunk is copied into it. The @var{merge} argument is only used if @var{buffer} is
@code{nil} or unsupplied. It true, the default, the new chunk is merged with the contents
of declarative memory; if false a new chunk is added, even if it is equal to one already
in declarative memory. If @var{json} contains sub-objects, the corresponding chunks
pointed at by the top level chunk are always added to declarative memory and not placed
into a buffer; these inner chunks are either added or merged according to the value of
@var{merge}. Returns the name of the new chunk.

Not that there needs to be a current ACT-R model to call @code{chunkify}.

Becaue of Lisp package issues it is usually best to load ACT-R before Voorhees.

The system keys @samp{@@id} and @samp{@@value} may be used to denote shared structure in a
JSON object being chunkified. If a JSON object contains a binding of @samp{@@id}, the
value of that binding, which must be a string or non-negative integer, names the
corresponding JSON object. It can be referred to elsewhere in the parent object by using
@samp{@@value} with the same name. Such an sub-object containing an @samp{@@value} must be
the only binding in that sub-object, the cited sub-object defined elsewhere replacing it.

TODO: this clearly needs an example."
  (ensure-act-r-7)
  (when (or (atom json) (atom (first json)))
    (error "Currently Voorhees can only convert non-empty JSON objects to ACT-R chunks (~S)" json))
  (let ((*chunk-table* (make-hash-table :test #'equal)))
    (chunkify-ids json merge t)
    (%chunkify json buffer overwrite merge t)))

(defun slotify (symbol)
  (when (notevery #'alphanumericp (symbol-name symbol))
    (setf symbol (format-symbol t "~A~A" +non-alphanumeric-slot-prefix+ symbol)))
  (extend-possible-slots symbol nil)
  symbol)

(defun %chunkify (json buffer overwrite merge allow-ids)
  (iter (for (key . value) :in json)
        (when (member key '(:@id :@value))
          (unless (or (eq key :@id) (eql (length json) 1))
            (error "@value may only appear as the sole key in an object (~S)" json))
          (if allow-ids
              (return-from %chunkify (gethash value *chunk-table*))
              (error "@ids and @values may not be nested or duplicated in objects named by an @id (~S)."
                     json)))
        (collect (slotify key) :into slots)
        (collect (etypecase value
                   (cons (%chunkify value nil nil merge allow-ids))
                   (number value)
                   (string value)
                   ((member :true :false :null) value)
                   (vector (error "Currently Voorhees can't convert JSON arrays to ACT-R chunks (~S)" value)))
          :into values)
        (finally (return (let ((description (list (iter (for s :in slots)
                                                        (for v :in values)
                                                        (nconcing (list s v))))))
                           (cond (buffer
                                  (let ((chunk (first (define-chunks-fct description))))
                                    (prog1
                                        (if overwrite
                                            (overwrite-buffer-chunk buffer chunk :requested nil)
                                            (set-buffer-chunk buffer chunk :requested nil))
                                      (purge-chunk-fct chunk))))
                                 (merge (first (merge-dm-fct description)))
                                 (t (first (add-dm-fct description)))))))))

(defun chunkify-ids (json merge &optional top-level)
  (let ((id (assoc :@id json)))
    (cond (id (when top-level
                (error "An @id (~S) may not appear at the top level of an object (~S)."
                       (cdr id) json))
              (when (shiftf (gethash (cdr id) *chunk-table*)
                            (%chunkify (remove id json) nil nil merge nil))
                (error "Duplicate @id ~S." value)))
          (t (iter (for (nil . value) :in json)
                   (when (listp value)
                     (chunkify-ids value merge)))))))


;;; STAP

(defun stapify (json)
  "Further decodes a piece of decoded JSON according to STAP's convention of using JSON
arrays to represent object-like values that preserve order."
  (if (vectorp json)
      (iter (for elem :in-vector json)
            (collect
                (cond ((vectorp elem)
                       (unless (eql (length elem) 2)
                         (error "Unexpected subvector in STAP: ~S" elem))
                       (cons (string-to-symbol (elt elem 0)) (elt elem 1)))
                      (t (cons (string-to-symbol elem) t)))))
      json))
