(defun i2c-scan ()
  (dotimes (p 127)
    (with-i2c (str p)
      (when str (format t "#x~x~%" p)))))
