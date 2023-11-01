(defstruct json-tcp-stream socket log-file log-json)

(defun open-json-tcp (port &key host timeout log-file log-json)
  (check-type port (integer 1) "a positive integer")
  (check-type host (or string (integer 1) vector null)
              "a string, positive integer, vector of non-negative integers or NIL")
  (check-type timeout (or (integer 0) null) "a positive integer or NIL")
  (check-type log-file (or pathname string stream null) "a pathname, string, stream or NIL")
  (check-type repeat (or (member nil t) function))
  (when log-file
    (setf log-file (merge-pathnames log-file)))
