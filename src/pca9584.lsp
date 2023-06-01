;;; Driver for the TCA9548A I2C Multiplexer
;;; Dave Astels

(defvar PCA9548A_ADDRESS #x70)

(defun pca9548-select (channel)
  (when (<= channel 7)
    (with-i2c (s PCA9548A_ADDRESS)
      (write-byte (ash #x01 channel) s))))

(defun pca9548-scan ()
  (format t "~%PCAScanner ready!")
  (dotimes (i 8)
    (pca9548-select i)
    (format t "~%PCA Port #~a" i)
    (dotimes (a 128)
      (unless (= a PCA9548A_ADDRESS)
        (with-i2c (s a)
          (when s
            (format t "~%Found I2C #x~x" a))))))
  (format t "~%Done"))
