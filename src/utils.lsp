;;; Returns a signed 16-bit integer from two bytes read from a stream, LSB first

(defun s16 (s)
  (let ((d (logior (read-byte s) (ash (read-byte s) 8))))
    (- d (ash (logand d #x8000) 1))))
