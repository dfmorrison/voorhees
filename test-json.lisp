(setf xxx
      (with-open-file (s "~/w/voorhees/test-json.json")
        (iter (for json := (vh:read-json s))
              (print json)
              (while (not (eql json 22)))
              (collect json))))

(with-open-file (s "~/w/voorhees/test-json.json")
  (print (vh:read-json s))
  (print (read-char s)))

