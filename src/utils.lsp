;;; Returns a 16-bit integer from two bytes read from a stream, LSB first

(defun read-word (s)
  (logior (read-byte s) (ash (read-byte s) 8)))

(defun write-word (value s)
  (write-byte (logand value #xFF) s)
  (write-byte (logand (ash value -8) #xFF) s))
