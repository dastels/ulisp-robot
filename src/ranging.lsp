;;; Ranging code
;;; Manage the 8 range sensors
;;; Dave Astels 2023

(defvar range-results '((0 VL6180X_ERROR_NONE)
                        (0 VL6180X_ERROR_NONE)
                        (0 VL6180X_ERROR_NONE)
                        (0 VL6180X_ERROR_NONE)
                        (0 VL6180X_ERROR_NONE)
                        (0 VL6180X_ERROR_NONE)
                        (0 VL6180X_ERROR_NONE)
                        (0 VL6180X_ERROR_NONE)))


(defun initialize-range-sensors ()
  (dotimes (channel 8)
    (pca9548-select channel)
    (vl6180-init)))


(defun update-range (channel)
  (pca9548-select channel)
  (let ((result-pair (nth channel range-results))
        (range (vl6180-read-range))
        (status (vl6180-read-range-status)))
    (setf (first result-pair) range
          (second result-pair) status)))


(defun update-ranges ()
  (dotimes (channel 8)
    (update-range channel)))
