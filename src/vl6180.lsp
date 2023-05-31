;;; VL6180X driver for the Arduino platform.  It is designed specifically to work with the
;;; Adafruit VL6180X breakout: http://www.adafruit.com/products/3316
;;; Dave Astels

(defvar VL6180X_DEFAULT_I2C_ADDR #x29) ; The fixed I2C address

(defvar VL6180X_REG_MODEL_ID #x0000)       ; Device model identification number
(defvar VL6180X_REG_INT_CONFIG #x0014)     ; Interrupt configuration
(defvar VL6180X_REG_INT_CLEAR #x0015)      ; Interrupt clear bits
(defvar VL6180X_REG_FRESHLY_RESET #x0016)  ; Fresh out of reset bit
(defvar VL6180X_REG_SYSRANGE_START #x0018) ; Trigger Ranging
(defvar VL6180X_REG_RANGE_OFFSET #x0024)   ; Part to part range offset
(defvar VL6180X_REG_SYSALS_START #x0038)   ; Trigger Lux Reading
(defvar VL6180X_REG_SYSALS_ANALOGUE_GAIN #x003F) ; Lux reading gain
(defvar VL6180X_REG_SYSALS_PERIOD_HI #x0040) ; Integration period for ALS mode, high byte
(defvar VL6180X_REG_SYSALS_PERIOD_LO #x0041) ; Integration period for ALS mode, low byte
(defvar VL6180X_REG_RESULT_RANGE_STATUS #x004D)     ; Specific error codes
(defvar VL6180X_REG_RESULT_INT_STATUS #x004F) ; Interrupt status
(defvar VL6180X_REG_RESULT_ALS_VAL #x0050)          ; Light reading value
(defvar VL6180X_REG_RESULT_RANGE_VAL #x0062)        ; Ranging reading value
(defvar VL6180X_REG_SLAVE_DEVICE_ADDRESS #x212)   ; I2C Slave Device Address

(defvar VL6180X_ALS_GAIN_1 #x06)    ; 1x gain
(defvar VL6180X_ALS_GAIN_1_25 #x05) ; 1.25x gain
(defvar VL6180X_ALS_GAIN_1_67 #x04) ; 1.67x gain
(defvar VL6180X_ALS_GAIN_2_5 #x03)  ; 2.5x gain
(defvar VL6180X_ALS_GAIN_5 #x02)    ; 5x gain
(defvar VL6180X_ALS_GAIN_10 #x01)   ; 10x gain
(defvar VL6180X_ALS_GAIN_20 #x00)   ; 20x gain
(defvar VL6180X_ALS_GAIN_40 #x07)   ; 40x gain

(defvar VL6180X_ERROR_NONE 0)        ; Success!
(defvar VL6180X_ERROR_SYSERR_1 1)    ; System error
(defvar VL6180X_ERROR_SYSERR_5 5)    ; Sysem error
(defvar VL6180X_ERROR_ECEFAIL 6)     ; Early convergence estimate fail
(defvar VL6180X_ERROR_NOCONVERGE 7)  ; No target detected
(defvar VL6180X_ERROR_RANGEIGNORE 8) ; Ignore threshold check failed
(defvar VL6180X_ERROR_SNR 11)        ; Ambient conditions too high
(defvar VL6180X_ERROR_RAWUFLOW 12)   ; Raw range algo underflow
(defvar VL6180X_ERROR_RAWOFLOW 13)   ; Raw range algo overflow
(defvar VL6180X_ERROR_RANGEUFLOW 14) ; Raw range algo underflow
(defvar VL6180X_ERROR_RANGEOFLOW 15) ; Raw range algo overflow

;; Define some additional registers mentioned in application notes and we use
;; period between each measurement when in continuous mode
(defvar INTERMEASUREMENT_PERIOD #x001B) ; P19 application notes


(defun vl6180-write-address (address s)
  (write-byte (logand (ash address 8) #xFF) s)
  (write-byte (logand address #xFF) s))


(defun vl6180-read8 (address)
  (with-i2c (s VL6180X_DEFAULT_I2C_ADDR)
    (vl6180-write-address address s)
    (restart-i2c s 1)
    (read-byte s)))


(defun vl6180-read16 (address)
  (with-i2c (s VL6180X_DEFAULT_I2C_ADDR)
    (vl6180-write-address address s)
    (restart-i2c s 2)
    (let* ((high (read-byte s))
           (low (read-byte s)))
      (logior (logand (ash high -8) #xFF)
              (logand low #xFF)))))


(defun vl6180-write8 (address data)
  (with-i2c (s VL6180X_DEFAULT_I2C_ADDR)
    (vl6180-write-address address s)
    (write-byte data s)))


(defun vl6180-write16 (address data)
  (with-i2c (s VL6180X_DEFAULT_I2C_ADDR)
    (vl6180-write-address address s)
    (write-byte (logand (ash data 8) #xFF) s)
    (write-byte (logand data #xFF) s)))


(defun vl6180-check-id ()
  (= (vl6180-read8 VL6180X_REG_MODEL_ID) #xB4))


(defun vl6180-just-reset ()
  (if (zerop (logand (vl6180-read8 VL6180X_REG_FRESHLY_RESET) #x01))
      nil
      t))


(defun vl6180-clear-just-reset ()
  (vl6180-write8 VL6180X_REG_FRESHLY_RESET #x00))


;;; Load the settings for proximity/distance ranging

(defun vl6180-load-settings ()
  (let ((settings '((#x0207 #x01)
                    (#x0208 #x01)
                    (#x0096 #x00)
                    (#x0097 #xfd)
                    (#x00E3 #x00)
                    (#x00E4 #x04)
                    (#x00E5 #x02)
                    (#x00e6 #x01)
                    (#x00E7 #x03)
                    (#x00F5 #x02)
                    (#x00D9 #x05)
                    (#x00DB #xCE)
                    (#x00DC #x03)
                    (#x00DD #xF8)
                    (#x009F #x00)
                    (#x00A3 #x3C)
                    (#x00B7 #x00)
                    (#x00BB #x3C)
                    (#x00B2 #x09)
                    (#x00CA #x09)
                    (#x0198 #x01)
                    (#x01B0 #x17)
                    (#x01AD #x00)
                    (#x00FF #x05)
                    (#x0100 #x05)
                    (#x0199 #x05)
                    (#x01A6 #x1B)
                    (#x01AC #x3E)
                    (#x01A7 #x1F)
                    (#x0030 #x00)
                    (#x0011 #x10)
                    (#x010A #x30)
                    (#x003F #x46)
                    (#x0031 #xFF)
                    (#x0041 #x63)
                    (#x002E #x01)
                    (#x001B #x09)
                    (#x003E #x31)
                    (#x0014 #x24))))
    (dolist (address-data settings)
      (vl6180-write8 (first address-data) (second address-data)))))


(defun vl6180-init ()
  (when (and (vl6180-check-id)
             (vl6180-just-reset))
    (vl6180-load-settings)
    (vl6180-clear-just-reset)))


;;; Single shot ranging. Be sure to check the return of readRangeStatus to before using the return value!
;;; Returns the distance in millimeters if valid

(defun vl6180-read-range ()
  ;; wait for device to be ready for range measurement
  (loop
    (when (logand (vl6180-read8 VL6180X_REG_RESULT_RANGE_STATUS) #x01)
      (return)))
  ;; Start a range measurement
  (vl6180-write8 VL6180X_REG_SYSRANGE_START #x01)
  ;; Poll until bit 2 is set
  (loop
    (when (= (logand (vl6180-read8 VL6180X_REG_RESULT_INT_STATUS) #x04) #x04)
      (return)))
  ;; read range in mm
  (let ((range (vl6180-read8 VL6180X_REG_RESULT_RANGE_VAL)))
    ;; clear interrupt
    (vl6180-write8 VL6180X_REG_INT_CLEAR #x07)
    range))


;;; Single shot lux measurement
;;; param:  gain Gain setting, one of VL6180X_ALS_GAIN_*
;;; Returns Lux reading

(defun vl6180-read-lux (gain)
  (let (lux)
    ;; IRQ on ALS ready
    (vl6180-write8 VL6180X_REG_INT_CONFIG (logior (logand (vl6180-read8 VL6180X_REG_INT_CONFIG) (lognot #x38)) #x20))
    ;; 100 ms integration period
    (vl6180-write8 VL6180X_REG_SYSALS_PERIOD_HI 0)
    (vl6180-write8 VL6180X_REG_SYSALS_PERIOD_LO 100)
    ;; analog gain
    (when (> gain VL6180X_ALS_GAIN_40)
      (setq gain VL6180X_ALS_GAIN_40))
    (vl6180-write8 VL6180X_REG_SYSALS_ANALOGUE_GAIN (logior gain #x40))
    ;; start ALS
    (vl6180-write8 VL6180X_REG_RESULT_ALS_VAL #x01)
    ;; Poll until "New Sample Ready threshold event" is set
    (loop
      (when (= (logand (ash (vl6180-read8 VL6180X_REG_RESULT_INT_STATUS) -3) #x07) #x04)
        (return)))
    ;; read lux!
    (setq lux (float (vl6180-read16 VL6180X_REG_RESULT_ALS_VAL)))
    ;; clear interrupt
    (vl6180-write8 VL6180X_REG_INT_CLEAR #x07)
    ;; calibrated count/lux
    (/ (* lux 0.32)
       (case gain
         (VL6180X_ALS_GAIN_1 1)
         (VL6180X_ALS_GAIN_1_25 1.25)
         (VL6180X_ALS_GAIN_1_67 1.67)
         (VL6180X_ALS_GAIN_2_5 2.5)
         (VL6180X_ALS_GAIN_5 5)
         (VL6180X_ALS_GAIN_10 10)
         (VL6180X_ALS_GAIN_20 20)
         (VL6180X_ALS_GAIN_40 40)))))


;;; Request ranging success/error message - retreive after ranging
;;; Returns One of possible VL6180X_ERROR_* values

(defun vl6180-read-range-status ()
  (ash (vl6180-read8 VL6180X_REG_RESULT_RANGE_STATUS) -4))


;;; Start a single shot ranging. The caller of this should have code
;;; that waits until the read completes, by either calling
;;; waitRangeComplete or calling isRangeComplete until it
;;; returns true.  And then the code should call readRangeResult
;;; to retrieve the range value and clear out the internal status.
;;; Returns true if range completed

(defun vl6180-start-range ()
  ;; wait for device to be ready for range measurement
  (loop
    (when (logand (vl6180-read8 VL6180X_REG_RESULT_RANGE_STATUS) #x01)
      (return)))
  (vl6180-write8 VL6180X_REG_SYSRANGE_START #x01))


;;; Check to see if the range command completed.
;;; Returns true if range completed.

(defun vl6180-is-range-complete ()
  (if (zerop (logand (vl6180-read8 VL6180X_REG_RESULT_INT_STATUS s) #x04))
      nil
      t))


;;; Wait until Range completed
;;; Returns true if range completed.

(defun vl6180-wait-range-complete ()
    (loop
      (unless (zerop (logand (vl6180-read8 VL6180X_REG_RESULT_INT_STATUS s) #x04))
        (return))))


;;; Return results of read request, also clears out the interrupt.
;;; Be sure to check the return of readRangeStatus to before using
;;; the return value!
;;; Returns the distance in millimeters if valid

(defun vl6180-read-range-result ()
  (let (range)
    ;; read range in mm
    (setq range (vl6180-read8 VL6180X_REG_RESULT_RANGE_VAL))
    ;; clear interrupt
    (vl6180-write8 VL6180X_REG_INT_CLEAR #x07)
    range))


;;;Start continuous ranging
;;; param:  period_ms Optional Period between ranges in ms.  Values will
;;;         be rounded down to 10ms units with minimum of 10ms.  Default is 50

(defun vl6180-start-range-continuous (&optional (period-ms 50))
  (let ((period-reg
          (if (> period-ms 10)
              (if (< period-ms 2550)
                  (- (/ period-ms 10) 1)
                  254)
              0)))
    (vl6180-write8 INTERMEASUREMENT_PERIOD period-reg)
    (vl6180-write8 VL6180X_REG_SYSRANGE_START #x03)))


;;; Stop continuous range operation

(defun vl6180-stop-range-continuous ()
  (vl6180-write8 VL6180X_REG_SYSRANGE_START #x01))

;; readRangeResult and isRangeComplete apply here is well

(defun vl6180-set-offset (offset)
  )


(defun test-vl6180 ()
  (vl6180-init)
  (print "Inited")
  (dotimes (i 100)
    (let ((range (vl6180-read-range))
          (status (vl6180-read-range-status)))
      (cond ((= status VL6180X_ERROR_NONE)
             (format t "~%Range: ~a" range))
            ((and (>= status VL6180X_ERROR_SYSERR_1) (<= status VL6180X_ERROR_SYSERR_5))
             (format t "~%System error"))
            ((= status VL6180X_ERROR_ECEFAIL)
             (format t "~%ECE failure"))
            ((= status VL6180X_ERROR_NOCONVERGE)
             (format t "~%No convergence"))
            ((= status VL6180X_ERROR_RANGEIGNORE)
             (format t "~%Ignoring range"))
            ((= status VL6180X_ERROR_SNR)
             (format t "~%Signal/Noise error"))
            ((= status VL6180X_ERROR_RAWUFLOW)
             (format t "~%Raw reading underflow"))
            ((= status VL6180X_ERROR_RAWOFLOW)
             (format t "~%Raw reading overflow"))
            ((= status VL6180X_ERROR_RANGEUFLOW)
             (format t "~%Range reading underflow"))
            ((= status VL6180X_ERROR_RANGEOFLOW)
             (format t "~%Range reading overflow"))))
    (delay 50)))
