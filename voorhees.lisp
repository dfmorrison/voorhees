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
  (:export #:*actr-package*
           #:*default-at-keys*
           #:*default-float-elide-trailing-zeros*
           #:*default-float-format*
           #:*default-float-maximum-fixed*
           #:*default-float-minimum-fixed*
           #:*default-float-precision*
           #:*default-float-upper-case-exponent*
           #:*default-object-key-package*
           #:chunkify
           #:describe-json
           #:json-string
           #:parse-json
           #:read-json
           #:run-model
           #:write-json))

(in-package :voorhees)



(defparameter *default-intern-keys* t
  "@code{t}, @code{nil} or a package designator, the value used for the @var{intern}
argument to @code{read-json} or @code{parse-json} if none is explicitly supplied,
indicating how to deal with keys in JSON objects. Initially @code{t}.")

(defparameter *default-at-keys* t
  "A generalized boolean, the value used for the @var{at-keys} argument to
@code{read-json} or @code{parse-json} if none is explicitly supplied, indicating how to
deal with keys in JSON objects that begin with @samp{@@}. Initially @code{t}.")

(defparameter *default-float-format* 'single-float
  "An atomic type specifier for a subtype of @code{float}, the value used for the
@var{float} argument to @code{read-json} or @code{parse-json} if none is explicitly
supplied, indicating what Lisp format to use to represent floating point numbers read from
JSON values. Initially @code{single-float}.")

(defparameter *default-float-minimum-fixed* 1.0e-3
  "A positive, real number, the value used for the @var{minimum-fixed} argument to
@code{write-json} or @code{json-string} if none is explicitly supplied, used to determine
whether to use fixed format or scientific notation when writing floating point numbers as
JSON. Intially @code{1.0e-3}.")

(defparameter *default-float-maximum-fixed* 1.0e+7
  "A positive, real number, the value used for the @var{maximum-fixed} argument to
@code{write-json} or @code{json-string} if none is explicitly supplied, used to determine
whether to use fixed format or scientific notation when writing floating point numbers as
JSON. Intially @code{1.0e+7}.")

(defparameter *default-float-precision* 4
  "A positive integer, the value used for the @var{precision} argument to
@code{write-json} or @code{json-string} if none is explicitly supplied, used to determine
the number of digits to be displayed after the decimal point when formating a floating
point number in JSON. Initially @code{4}.")

(defparameter *default-float-elide-trailing-zeros* t
  "A generalized boolean, the value used for the @var{elide} argument to @code{write-json}
or @code{json-string} if none is explicitly supplied, used to determine whether to omit
trailing zeros when formating point number in JSON. Initially @code{t}.")

(defparameter *default-float-upper-case-exponent* nil
  "A generalized boolean, the value used for the @var{upper-case-exponent} argument to
@code{write-json} or @code{json-string} if none is explicitly supplied, used to determine
whether to use an upper case @samp{E} or lower case @samp{e} when writing floating point
numbers in JSON as scientific notation. Initially @code{nil}.")

(defvar *intern-keys*)
(defvar *at-keys*)
(defvar *float-format*)
(defvar *float-minimum-fixed*)
(defvar *float-maximum-fixed*)
(defvar *float-precision*)
(defvar *float-elide-trailing-zeros*)
(defvar *float-upper-case-exponent*)

(defmacro define-json-reader (name (&rest args) &body body)
  ;; This won't work for a general argument list, but it's good enough for our purposes.
  `(defun ,name (,@args
                 ,@(unless (member '&key args) '(&key))
                 ((:intern *intern-keys*) *default-intern-keys*)
                 ((:at-keys *at-keys*) *default-at-keys*)
                 ((:float *float-format*) *default-float-format*))
     ,(and (stringp (first body)) (pop body))
     (let ((st-json:*decode-objects-as* :jso))
       ,@body)))

(define-json-reader read-json (stream)
  "===summary===
In Voorhees a JSON value is represented in Lisp as nestings of atoms, a-lists and simple
vectors, isomorphic to the JSON value, as follows.

A JSON @strong{array} is represented by a Lisp general vector, each element of which is
the Lisp representation of the JSON value that is an element of the JSON array.

A JSON @strong{object} is represented by a Lisp a-list, the car of each element
corresponding to the string key of the JSON object, and the cdr to the corresponding JSON
value in the object. The JSON strings that are the keys of the object members typically
are converted to interned Lisp symbols, but can be left as Lisp strings if preferred. The
user has control over the package in which these symbols are interned. The JSON values of
the objects named by these keys are converted to their Lisp representations and are the
cdrs of the elements of the a-list.

It is often convenient to treat keys beginning with @samp{@@} as system keys, reserved for
special purposes. Some uses of Voorhees depend upon treating such keys specially, as do
some other uses of JSON. As initially configured Voorhees always interns such keys in the
keyword package, but this can be turned off. If any special use is being made of some of
these @samp{@@} keys it is strongly recommended that keys beginning with @samp{@@} always
be viewed as so reserved.

When these keys, whether normal keys or @samp{@@} keys, are interned symbols the case of
their print names are usually different than that of the JSON strings:
@itemize
@item
if the JSON string contains lower case letters, as determined by the usual
Lisp predicate @code{lower-case-p}, but no upper case
letters (@code{upper-case-p}), it is converted to all upper case, with the usual
Lisp function @code{string-upcase}, and then interned in the appropriate package;

@item
if the JSON string contains upper case letters, but no lower case letters,
it is converted to all lower case and then interned in the appropriate package;

@item
otherwise the JSON string either contains no letters, or contains a mixture
of upper case and lower case letters, and is interned as is.
@end itemize
@noindent
With this convention the usual lower case only JSON keys are represented by the usual
upper case only Lisp symbols. Note that a Lisp symbol with lower case letters,
representing an upper case or mixed case JSON key string, will be read and written by Lisp
using vertical bar or backslash notation. When keys are instead represented by Lisp
strings no case conversion is performed.

A JSON @strong{string}, other than a key in an object, is represented by the
corresponding Lisp string, with no alteration of case.

A JSON @strong{number} without a decimal point is represented by a Lisp integer.

A JSON @strong{number} with a decimal point is represented by a Lisp float. Note that in
Lisp it will typically be a fixed-precision, binary representation of the number and thus
not necessarily exactly equal to the arbitrary, decimal representation in JSON. The
particular floating point format used is configurable, and is initially
@code{single-float}.

When writing a Lisp representation of a JSON value back to JSON some control is provided
over the represention used, including precision.

The JSON @strong{special values} @code{true}, @code{false} and @code{null} are
represented by the Lisp keywords @code{:true}, @code{:false} and @code{:null},
respectively. In particular @code{false} and @code{null} are @emph{not} represented by
Lisp @code{nil}, which instead represents a JSON empty object, @code{@{@}}. A JSON
empty array, @code{[]}, is represented by an empty Lisp vector, @code{#()}.

This representation of JSON values in Lisp is essentially reversable. When writing back
such a value back to JSON it will be equivalent to the original version read. The only
differences are: floating point numbers of high precision may be slightly altered because
of the conversion to and from a fixed-precision, binary form which is typically not an
issue for numbers with only a few digits of precision; and whitespace between JSON tokens
will be removed (of course, whitespace within JSON strings is retained).

As an example of the correspondence between JSON values and Lisp form, with Voorhees's
initial configuration, the JSON value
@example
@group
 @{ \"key1\": \"a value\",
   \"KEY2\": [ true, 3.14, @{@}, @{ \"Key3\": null@}, false],
   \"@@id\": 17,
   \"key4\": [] @}
@end group
@end example
is represented in Lisp by
@example
@group
 ((KEY1 . \"a value\")
  (|key2| . #(:TRUE 3.14 NIL ((|Key3| . :NULL)) :FALSE))
  (:@@ID . 17)
  (KEY4 . #()))
@end group
@end example
===endsummary===
Reads a JSON value from the character input stream @var{stream}, skipping any leading
whitespace and reading and discarding any trailing whitespace, and returns its Lisp
representation.

This function will block until a complete JSON value has been consumed. In addition, if
the value is none of an object, an array or a string, and the stream has not reached end
of file, @code{read-json} will need to peek at the next character to ensure it has
finished reading the value. For this reason, when reading from a semi-interactive stream,
such as from a TCP socket, it is highly recommended that each JSON value be followed by a
newline or other whitespace character.

If @var{intern} is @code{t} keys of JSON objects will be represented as symbols interned
in the package that is the current value of @code{*package*}. If @code{nil} keys will be
read as Lisp strings. Otherwise @var{intern} should be a package designator, and keys will
be interned in that package. If @var{intern} is not supplied the current value of
@code{*default-intern-keys*} is used; it is initially @code{t}.

If the generalized boolean @var{at-keys} is true keys in JSON objects that start with
@samp{@@} will always be interned in the keyword package, no matter what the value of
@var{intern}. Otherwise keys starting with @samp{@@} will be treated the same as any other
keys. If @var{at-keys} is not supplied the current value of @code{*default-at-keys*} is
used; it is initially @code{t}.

If @var{float} is supplied it should be one of the atomic type specifiers
@code{short-float}, @code{single-float}, @code{double-float} or @code{long-float}. Any
numbers in the JSON value that contain a decimal point are read as Lisp floating point
numbers of this type, subject to the usual collapsing of floating point types if all are
not supported by a particular implementation. If @var{float} is not supplied it defaults
to the current value of @code{*default-float-format*}; it is initially
@code{single-float}.

Whenever @var{intern} or @var{at-keys} leads to a key being interned in a package it is
subject to the case conversion rules described above.

An error will be signalled if a well-formed JSON value cannot be read from @var{stream};
if @var{stream} is not an open character input stream; if @var{intern} is supplied and is
not @code{t}, @code{nil} nor a package designator for an existing package; if @var{float}
is not a suitable type specifier; or if any of the usual variety of runtime I/O errors
possible occurs with @var{stream}.

@example
@group
 (with-input-from-string (s
      \"@{ \\\"description\\\": @{
             \\\"name\\\": \\\"Wilbur\\\",
             \\\"species\\\": \\\"Equus ferus caballus)\\\" @},
         \\\"coordinates\\\": @{
             \\\"x\\\": 1.2,
             \\\"y\\\": 3.4,
             \\\"z\\\": 5.6 @} @}\")
   (read-json s))
 @result{}
 ((DESCRIPTION (NAME . \"Wilbur\")
               (SPECIES . \"Equus ferus caballus)\"))
  (COORDINATES (X . 1.2) (Y . 3.4) (Z . 5.6)))
@end group
@end example
@noindent
For further examples see @code{parse-json}."
    (unpack-json (st-json:read-json stream)))

(define-json-reader parse-json (string &key (start 0) end junk-allowed)
  "Reads a JSON value from the string @var{string}, including any leading or trailing
whitespace, and returns two values: the Lisp representation of the JSON value and an index
into the string where parsing ended.

If @var{start} and/or @var{end} are supplied they should be integer indices into
@var{string} and only that portion of @var{string} bounded by them is considered.

The generalized boolean @var{junk-allowed} determines what happens if further
non-whitespace characters occur in the region of @var{string} bound by @var{start} and
@var{end}. If false and such characters appear an error is signaled; if true no error is
signalled and the second return value is the index of that first non-whitespace character
following the JSON value read. If not supplied @var{junk-allowed} defaults to @code{nil}.

The @var{intern}, @var{at-keys} and @var{float} arguments are interpreted as for the
@code{read-json} function.

Signals an error if no well-formed JSON value can be read from the designated region of
@var{string}; if @var{string} is not a string; if @var{start} or @var{end} are not
integers or are out of bounds for @var{string}; if @var{junk-allowed} is false and
non-whitespace characters are found after the JSON value; if @var{intern} is supplied and
is not @code{t}, @code{nil} nor a package designator for an existing package; or if
@var{float} is not a suitable type specifier.
@example
@group
 (values
   (parse-json \"@{ \\\"description\\\": @{
                      \\\"name\\\": \\\"Wilbur\\\",
                      \\\"species\\\": \\\"Equus ferus caballus)\\\" @},
                  \\\"coordinates\\\": @{
                      \\\"x\\\": 1.2,
                      \\\"y\\\": 3.4,
                \\\"z\\\": 5.6 @} @}\"))
 @result{}
 ((DESCRIPTION (NAME . \"Wilbur\")
               (SPECIES . \"Equus ferus caballus)\"))
  (COORDINATES (X . 1.2) (Y . 3.4) (Z . 5.6)))
@end group


@group
 (values (parse-json \" [ [ 3.1e22 ] ] \"))
 @result{}
 #(#(3.1e22))
@end group


@group
 (values (parse-json \" [ [ 3.1e22 ] ] \"
                :start 2 :end 14
                :float 'double-float))
 @result{}
 #(3.1d22)
@end group


@group
 (multiple-value-list
   (parse-json \"@{\\\"@@id\\\":19,\\\"x\\\":\\\"y\\\"@}\"))
 @result{}
 (((:@@ID . 19) (X . \"y\")) 18)
@end group


@group
 (values (parse-json \"@{\\\"@@id\\\":19,\\\"x\\\":\\\"y\\\"@}\"
                     :intern :keyword))
 @result{}
 ((:@@ID . 19) (:X . \"y\"))
@end group


@group
 (values (parse-json \"@{\\\"@@id\\\":19,\\\"x\\\":\\\"y\\\"@}\"
                     :intern nil))
 @result{}
 ((:@@ID . 19) (\"x\" . \"y\"))
@end group


@group
 (values (parse-json \"@{\\\"@@id\\\":19,\\\"x\\\":\\\"y\\\"@}\"
                     :at-keys nil))
 @result{}
 ((@@ID . 19) (X . \"y\"))
@end group


@group
 (values (parse-json \"@{\\\"@@id\\\":19,\\\"x\\\":\\\"y\\\"@}\"
                     :intern nil
                     :at-keys nil))
 @result{}
 ((\"@@id\" . 19) (\"x\" . \"y\"))
@end group
@end example"
  (multiple-value-bind (result index)
      (st-json:read-json-from-string string
                                     :start start
                                     :end end
                                     :junk-allowed-p junk-allowed)
    (values (unpack-json result) index)))

(defun write-json (object stream
                   &key (newline t) (finish t)
                     ((:minimum-fixed *float-minimum-fixed*) *default-float-minimum-fixed*)
                     ((:maximum-fixed *float-maximum-fixed*) *default-float-maximum-fixed*)
                     ((:precision *float-precision*) *default-float-precision*)
                     ((:elide *float-elide-trailing-zeros*) *default-float-elide-trailing-zeros*)
                     ((:upper-case-exponent *float-upper-case-exponent*) *default-float-upper-case-exponent*))
  "Writes a JSON value corresponding to the Lisp @var{object} to the character output
stream @var{stream}, and returns @var{object}.

If the generalized boolean @var{newline} is true, the default, it calls @code{terpri} on
@var{stream} after writing the JSON value. This is highly recommended when writing values
to semi-interactive streams, such as TCP sockets.

If the generalized boolean @var{finish} is true, the default, it calls
@code{finish-output} on @var{stream} after writing the JSON value, and a line termination
if request, to @var{stream}, flushing the output buffer. This is usually the necessary
when writing to semi-interactive streams, such as TCP sockets. In other situations if such
buffer flushing causes too much unnecessary overhead it can be suppressed by supplying a
value of @code{nil} for @var{finish}.

If @var{minimum-fixed} and/or @var{maximum-fixed} are supplied they should be positive,
real numbers. They determine whether floating point numbers, when converted to JSON, are
represented in fixed or exponential format. If the absolute value of the number is greater
than or equal to @var{minimum-fixed} and less than @var{maximum-fixed} fixed format will
be used, and otherwise exponential format. If @var{minimum-fixed} or @var{maximum-fixed}
are not supplied they default to the current values of
@code{*default-float-minimum-fixed*} and @code{*default-float-maximum-fixed*},
respectively; these are initially @code{1.0e-3} and @code{1.0e+7}.

If @var{precision} is supplied it should be a positive integer, the maximum number of
digits to be displayed after the decimal point when converting floating point numbers to
JSON. If the generalized boolean :elide is true then when converting floating point
numbers to JSON the last digit following the decimal point, but before the exponentiation
marker, if any, will be non-zero, even if this means fewer than @var{precision} digits
will be shown; an exception is made for the case where all digits following the decimal
point would be zero, in which case one will be shown. If not supplied @var{precision} and
@var{elide} default to the current values of @code{*default-float-precision*} and
@code{*default-float-elide-trailing-zeros*}, respectively. They are initially @code{4} and
@code{t}.

It is strongly recommended that @var{minimum-fixed} and @var{precision} only be assigned
values such that a number displayed in fixed notation will always have its first non-zero
digit appear in the printed representation.

If @var{upper-case-exponent} is supplied it determines whether an upper case @samp{E} or
lower case @samp{e} is used when displaying floating point numbers in exponential notation
in JSON. If not supplied it defaults to the current value of
@code{*default-float-upper-case-exponent*}, which is initially @code{nil}.

Signals an error if @var{object} is not the Lisp representation of a JSON value; if
@var{stream} is not an open character output stream; if @var{minimum-fixed} is not a
postive real, less than @var{maximum-fixed}; if @var{maximum-fixed} is not a positive
real greater than @var{minimum-fixed}; if precision is not a positive integer; or if any
of the usual variety of runtime I/O errors possible occurs with @var{stream}.
@example
@group
 (with-output-to-string (s)
   (write-json '((x . 3.2) (\\y . 4.1)) s))
 @result{}
 \"@{\\\"x\\\":3.2,\\\"Y\\\":4.1@}
 \"
@end group
@end example
@noindent
For further examples see @code{json-string}."
  (st-json:write-json (pack-json object) stream)
  (when newline
    (terpri stream))
  (when finish
    (finish-output stream))
  object)

(defun json-string (object &rest keys &key minimum-fixed maximum-fixed precision elide upper-case-exponent)
  "===lambda: (object &key minimum-fixed maximum-fixed precision elide upper-case-exponent)
Returns a string representation of the JSON value corresponding to the Lisp
@var{object}. The @var{minimum-fixed}, @var{maximum-fixed}, @var{precision}, @var{elide}
and @var{upper-case-exponent} arguments are interpreted as for @code{write-json}.

Signals an error if @var{object} is not the Lisp representation of a JSON value; if
@var{minimum-fixed} is not a postive real, less than @var{maximum-fixed}; if
@var{maximum-fixed} is not a positive real greater than @var{minimum-fixed}; or if
precision is not a positive integer.
@example
@group
 (json-string '((x . 3.2) (\\y . 4.1)) s)
 @result{}
 \"@{\\\"x\\\":3.2,\\\"Y\\\":4.1@}\"
@end group


@group
 (json-string '((:@@id . 1) (:id . 2) (@@n . 3) (n . 4) (\"num\" . 5)))
 @result{}
 \"@{\\\"@@id\\\":1,\\\"id\\\":2,\\\"@@n\\\":3,\\\"n\\\":4,\\\"num\\\":5@}\"
@end group


@group
 (json-string '((and . ((|and| . ((|aNd| . nil)))))))
 @result{}
 \"@{\\\"and\\\":@{\\\"AND\\\":@{\\\"aNd\\\":@{@}@}@}@}\"
@end group


@group
 (json-string #(3.14159265359 7.2973525664d-3 1.0))
 @result{}
 \"[3.1416,0.0073,1.0]\"
@end group


@group
 (json-string #(3.14159265359 7.2973525664d-3 1.0)
              :elide nil)
 @result{}
 \"[3.1416,0.0073,1.0000]\"
@end group


@group
 (json-string #(3.14159265359 7.2973525664d-3 1.0)
              :precision 2)
 @result{}
 \"[3.14,0.01,1.0]\"
@end group


@group
 (json-string #(3.14159265359 7.2973525664d-3 1.0)
              :minimum-fixed 0.01)
 @result{}
 \"[3.1416,7.2974e-3,1.0]\"
@end group


@group
 (json-string #(3.14159265359 7.2973525664d-3 1.0)
              :maximum-fixed 2
              :upper-case-exponent t)
 @result{}
 \"[3.1416E+0,0.0073,1.0]\"
@end group
@end example"
  (declare (ignore minimum-fixed maximum-fixed precision elide upper-case-exponent))
  (with-output-to-string (s)
    (apply #'write-json object s :newline nil :finish nil keys)))

(defun unpack-json (json)
  (etypecase json
    ((or keyword number string) json)
    (st-json:jso (iter (for (key . value) :in (st-json::jso-alist json))
                       (collect (cons (maybe-intern-key key) (unpack-json value)))))
    (list (map 'vector #'unpack-json json))))

(defun maybe-intern-key (string)
  (cond ((and *at-keys* (not (zerop (length string))) (eql (char string 0) #\@))
         (intern (flip-string-case-if-uniform string) :keyword))
        (*intern-keys* (intern (flip-string-case-if-uniform string)
                               (if (eq *intern-keys* t) *package* *intern-keys*)))
        (t string)))

(defun pack-json (json)
  (etypecase json
    ((or keyword number string) json)
    (list (st-json::make-jso
           :alist (iter (for (key . value) :in json)
                        (collect (cons (etypecase key
                                         (symbol (flip-string-case-if-uniform (symbol-name key)))
                                         (string key))
                                       (pack-json value))))))
    ;; the vector clause must come after the string clause, since a string is a vector
    (vector (map 'list #'pack-json json))))

(defun flip-string-case-if-uniform (string)
  (iter (with lower-case-seen := nil)
        (with upper-case-seen := nil)
        (for c :in-string string)
        (until (and lower-case-seen upper-case-seen))
        (cond ((lower-case-p c) (setf lower-case-seen t))
              ((upper-case-p c) (setf upper-case-seen t)))
        (finally (return (cond ((not (xor lower-case-seen upper-case-seen)) string)
                               (upper-case-seen (string-downcase string))
                               (t (string-upcase string)))))))


(defmethod st-json:read-json :around ((in stream) &optional (junk-allowed-p t))
  (declare (ignore in junk-allowed-p))
  (let ((*read-default-float-format* *float-format*))
    (call-next-method)))

(defparameter +float-scanner+
  (ppcre:create-scanner "^(-?\\d+\\.\\d\\d*?)0*((?:e[+-]?\\d+)?)?$"
                        :case-insensitive-mode t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Suppress the warning about the subsequent method redefinition.
  (when-let ((method (find-method #'st-json:write-json-element ()
                                  (mapcar #'find-class '(real t))
                                  nil)))
    (remove-method #'st-json:write-json-element method)))

(defmethod st-json:write-json-element ((element real) stream)
  (let ((s (if (or (zerop element)
                   (and *float-minimum-fixed*
                        *float-maximum-fixed*
                        (>= (abs element) *float-minimum-fixed*)
                        (< (abs element) *float-maximum-fixed*)))
               (format nil "~,vF" *float-precision* element)
               (format nil "~,v,,,,,vE"
                       *float-precision*
                       (if *float-upper-case-exponent* #\E #\e)
                       element))))
    (if *float-elide-trailing-zeros*
        (ppcre:register-groups-bind (pre post) (+float-scanner+ s)
          (princ pre stream)
          (princ post stream))
        (princ s stream))))



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
       ;; TODO replace the following with read-json, catching appropriate errors
       ;;      and dealing with them in some not too intrusive way
       (loop for (json success) = (multiple-value-list (read-multiline-json stream))
             while success
             when log-json do (write-log log-file "received: ~S" json)
             do (when-let ((response (funcall model-fn json)))
                  (when log-json
                    (write-log log-file "sending: ~S" response))
                  (write-json response stream)))
    (write-log log-file "run-model done")))

(define-constant +whitespace+ '(#\space #\newline #\tab #\return #\page) :test #'equal)

(defun read-multiline-json (stream)
  (loop with lines = (make-array 100 :element-type 'character
                                 :adjustable t :fill-pointer 0)
        for (line missing-newline) = (multiple-value-list (read-line stream nil))
        while line
        do (format lines "~A~:[~%~;~]" line missing-newline)
        do (handler-case
               (return (values (parse-json lines) t))
             (st-json:json-eof-error ()))
        finally (if (zerop (length (string-trim +whitespace+ lines)))
                    (return (values nil nil))
                    (error "End of file encountered while parsing multi-line JSON ~A"
                           lines))))

(defun copy-json (json)
  (etypecase json
    ((or symbol number) json)
    (string (copy-seq json))
    (cons (cons (copy-json (car json)) (copy-json (cdr json))))
    (vector (map 'simple-vector #'copy-json json))))


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
(define-actr-function schedule-event-relative)

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

(defun chunkify (json &key buffer overwrite (merge t)
                        time-delta time-in-ms (module :none) (priority 0) (output t))
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
@var{merge}.

Normally @code{chunkify} creates the chunk immediately. If @var{time-delta} is supplied
and not @code{nil} it should be a non-negative number, and an event to create the chunk is
schedule with ACT-R's @code{schedule-event-relative} at that time. If @var{time-delta} is
supplied and not @code{nil} the values of @var{time-in-ms}, @var{module}, @var{priority}
and @var{output} are passed to @code{schedule-event-relative}; otherwise those keyword
arguments are ignored.

If @var{time-delta} is not supplied or is @code{nil} @code{chunkify} returns the name of
the new chunk. Otherwise the event scheduled is returned.

Note that there needs to be a current ACT-R model to call @code{chunkify}.

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
  (labels ((deposit-chunk (jsn bffr owrt mrg)
             (format t "~2%*** ~S ~S ~S ~S~2%" jsn bffr owrt mrg)
             (let ((*chunk-table* (make-hash-table :test #'equal)))
               (format t "~2%*** foo~2%")
               (chunkify-ids jsn mrg t)
               (format t "~2%*** bar~2%")
               (%chunkify jsn bffr owrt mrg t)
               (format t "~2%*** baz~2%")
               )))
    (if time-delta
        (schedule-event-relative time-delta #'deposit-chunk
                                 ;; json is defensively copied in case the caller changes
                                 ;; it between when we return and when the event fires
                                 :params (list (copy-json json) buffer overwrite merge)
                                 :time-in-ms time-in-ms
                                 :module module
                                 :priority priority
                                 :output output)
        (deposit-chunk json buffer overwrite merge))))

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
