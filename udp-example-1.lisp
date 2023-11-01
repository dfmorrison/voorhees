(ql:quickload :usocket)

(defparameter +buffer-length+ 256)

(defun send-and-receive (message host port)
  "Open a connection to HOST on PORT, and send MESSAGE using UDP, then listen for and
return as a string a reply. MESSAGE should be a string, and consist of only 7-bit ASCII
characters, as also should the reply."
  (let (socket)
    (unwind-protect
         (progn
           (setf socket (usocket:socket-connect host port :protocol :datagram))
           (usocket:socket-send socket
                                (map '(vector (unsigned-byte 8)) #'char-code message)
                                (length message))
           (multiple-value-bind (buffer length)
               (usocket:socket-receive socket
                                       (make-array +buffer-length+
                                                   :element-type '(unsigned-byte 8))
                                       +buffer-length+)
             (map 'string #'code-char (subseq buffer 0 length))))
      (usocket:socket-close socket))))

      
