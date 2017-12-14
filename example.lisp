;;; Copyright (c) 2017 Carnegie Mellon University
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

;;; This is a trivial example of Voorhees in action. It uses the addition model from the
;;; first unit of the ACT-R tutorial to add numbers, the numbers being supplied over a
;;; TCP connection, and the result then being returned over that connection.
;;;
;;; To run the example first ensure you have Voorhees installed as suggested in its
;;; documentation, so that it can be loaded with QuickLisp. Then cd to a directory that
;;; contains an actr7 directory. (Alternatively you can set *actr7-parent*, below, to such
;;; a directory and run this from anywhere.) Then launch Lisp and load this file. It will
;;; start listenting for connections on port 9907 (you can change this port below). Then,
;;; from a separate terminal, send it a JSON object such as
;;;
;;; { "arg1": 3, "arg2": 5 }
;;;
;;; The ACT-R model will be run and the sum returned. For example, if you use netcat (nc)
;;; to connect the interaction in the second terminal might look something like this:
;;;
;;; $ nc localhost 9907
;;; {"arg1": 3, "arg2": 5}
;;; 8
;;; {"arg1": 3, "arg2": 2}
;;; 5
;;;

;; Run this in the CL-USER package 'cause that's the easiest place to use ACT-R.
(in-package :cl-user)

;; A couple of default values that can be changed if necessary.
(defparameter *actr7-parent* nil)

(defparameter *port* 9907)

;; Load ACT-R if it's not already present, and then Voorhees.
#-ACT-R
(load (merge-pathnames (make-pathname :directory '(:relative "actr7")
                                      :name "load-act-r"
                                      :type "lisp")
                       (or *actr7-parent* *default-pathname-defaults*))
      :verbose t
      :print nil)

(ql:quickload :voorhees)

;; Define the ACT-R model. This will generate a pair of warnings about slots in the goal
;; buffer not being accessed from productions; these may be safely ignored.

(clear-all)

(define-model addition

(sgp :esc t :lf .05)

(chunk-type count-order first second)
(chunk-type add arg1 arg2 sum count)

(add-dm
   (a ISA count-order first 0 second 1)
   (b ISA count-order first 1 second 2)
   (c ISA count-order first 2 second 3)
   (d ISA count-order first 3 second 4)
   (e ISA count-order first 4 second 5)
   (f ISA count-order first 5 second 6)
   (g ISA count-order first 6 second 7)
   (h ISA count-order first 7 second 8)
   (i ISA count-order first 8 second 9)
   (j ISA count-order first 9 second 10))

(P initialize-addition
   =goal>
      ISA         add
      arg1        =num1
      arg2        =num2
      sum         nil
  ==>
   =goal>
      ISA         add
      sum         =num1
      count       0
   +retrieval>
      ISA        count-order
      first      =num1
)

(P terminate-addition
   =goal>
      ISA         add
      count       =num
      arg2        =num
      sum         =answer
  ==>
   =goal>
      ISA         add
      count       nil
   !output!       =answer
)

(P increment-count
   =goal>
      ISA         add
      sum         =sum
      count       =count
   =retrieval>
      ISA         count-order
      first       =count
      second      =newcount
  ==>
   =goal>
      ISA         add
      count       =newcount
   +retrieval>
      ISA        count-order
      first      =sum
)

(P increment-sum
   =goal>
      ISA         add
      sum         =sum
      count       =count
    - arg2        =count
   =retrieval>
      ISA         count-order
      first       =sum
      second      =newsum
  ==>
   =goal>
      ISA         add
      sum         =newsum
   +retrieval>
      ISA        count-order
      first      =count
)

) ; end define-model


;; Define the function that will do the work.
(defun run-addition-model (args)
  "Creates a chunk from the parsed JSON args, sets it to be the goal, runs the model,
and then returns the value of the sum slot of the chunk in the goal buffer. Note that an
integer is itself a JSON value."
  (goal-focus-fct (vh:chunkify args))
  (run 10)
  (no-output (chunk-slot-value-fct (first (buffer-chunk goal)) 'sum)))

;; Run Voorhees and wait for connections, writing log information to the terminal, where
;; it will be interspersed with what ACT-R spits out.
(vh:run-model #'run-addition-model *port*
              :log-file *terminal-io*
              :log-json t)
