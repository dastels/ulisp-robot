;;; LIS3DH accelerometer interface
;;; Dave Astels based heavily on code frp, ulisp.com

(defvar LIS3DH_ADDRESS #x18)

;;; registers addresses

(defvar LIS3DH_REG_OUTADC1_L #x08)
(defvar LIS3DH_REG_WHOAMI #x0F)
(defvar LIS3DH_REG_TEMPCFG #x1F)
(defvar LIS3DH_REG_CTRL1 #x20)
(defvar LIS3DH_REG_CTRL3 #x22)
(defvar LIS3DH_REG_CTRL4 #x23)
(defvar LIS3DH_REG_CTRL5 #x24)
(defvar LIS3DH_REG_OUT_X_L #x28)
(defvar LIS3DH_REG_INT1SRC #x31)
(defvar LIS3DH_REG_CLICKCFG #x38)
(defvar LIS3DH_REG_CLICKSRC #x39)
(defvar LIS3DH_REG_CLICKTHS #x3A)
(defvar LIS3DH_REG_TIMELIMIT #x3B)
(defvar LIS3DH_REG_TIMELATENCY #x3C)
(defvar LIS3DH_REG_TIMEWINDOW #x3D)

;;; range values

(defvar LIS3DH_RANGE_16_G #x03)  ; +/- 16g
(defvar LIS3DH_RANGE_8_G #x02)  ; +/- 8g
(defvar LIS3DH_RANGE_4_G #x01)  ; +/- 4g
(defvar LIS3DH_RANGE_2_G #x00)  ; +/- 2g default value

;;; data rate values

(defvar LIS3DH_DATARATE_1344_HZ #x09)  ; 1.344 KHz
(defvar LIS3DH_DATARATE_400_HZ #x07)  ; 400Hz
(defvar LIS3DH_DATARATE_200_HZ #x06)  ; 200Hz
(defvar LIS3DH_DATARATE_100_HZ #x05)  ; 100Hz
(defvar LIS3DH_DATARATE_50_HZ #x04)  ; 50Hz
(defvar LIS3DH_DATARATE_25_HZ #x03)  ; 25Hz
(defvar LIS3DH_DATARATE_10_HZ #x02)  ; 10 Hz
(defvar LIS3DH_DATARATE_1_HZ #x01)  ; 1 Hz
(defvar LIS3DH_DATARATE_POWERDOWN #x00)

;;; Low power data rates
(defvar LIS3DH_DATARATE_LOWPOWER_1K6HZ #x08)
(defvar LIS3DH_DATARATE_LOWPOWER_5KHZ #x09)

;;; Other constants
(defvar STANDARD_GRAVITY  9.806)

;;; By default the sensor is in low-power mode, so to take readings you need to set the rate.
;;; The following routines lis3dh-rate takes a parameter from 0 to 9

(defun lis3dh-rate (x)
  (with-i2c (s LIS3DH_ADDRESS)
    (write-byte LIS3DH_REG_CTRL1 s)
    (write-byte (logior (ash x 4) 7) s)))

;;; Set the full-scale sensitivity of the accelerometer

(defun lis3dh-sensitivity (x)
  (with-i2c (s LIS3DH_ADDRESS)
    (write-byte LIS3DH_REG_CTRL4 s)
    (write-byte (ash x 4) s)))

;;; Get the acceleration data as a list of three signed integers

(defun lis3dh-xyz ()
  (with-i2c (s LIS3DH_ADDRESS)
    (write-byte (+ LIS3DH_REG_OUT_X_L #x80) s) ; Set top bit to read multiple bytes
    (restart-i2c s 6)
    (let (dat)
      (dotimes (i 3) (push (s16 s) dat))
      (reverse dat))))

;;; Get the values in g; it reads the currently selected full-scale sensitivity

(defun lis3dh-g3d ()
  (let ((fs (with-i2c (s LIS3DH_ADDRESS)
              (write-byte LIS3DH_REG_CTRL4 s)
              (restart-i2c s 1)
              (logand 3 (ash (read-byte s) -4)))))
    (mapcar (lambda (i) (/ i (ash 16384 (- fs)))) (lis3dh-xyz))))
