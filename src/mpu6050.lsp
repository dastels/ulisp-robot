;;; MPU6050 support

(defvar MPU6050-I2CADDR-DEFAULT #x68)   ; MPU6050 default i2c address w/ AD0 high
(defvar MPU6050-DEVICE-ID #x68)         ; The correct MPU6050_WHO_AM_I value

(defvar MPU6050-REG-SELF-TEST-X #x0D)       ; Self test factory calibrated values register
(defvar MPU6050-REG-SELF-TEST-Y #x0E)       ; Self test factory calibrated values register
(defvar MPU6050-REG-SELF-TEST-Z #x0F)       ; Self test factory calibrated values register
(defvar MPU6050-REG-SELF-TEST-A #x10)       ; Self test factory calibrated values register
(defvar MPU6050-REG-SMPLRT-DIV #x19)        ; sample rate divisor register
(defvar MPU6050-REG-CONFIG #x1A)            ; General configuration register
(defvar MPU6050-REG-GYRO-CONFIG #x1B)       ; Gyro specfic configuration register
(defvar MPU6050-REG-ACCEL-CONFIG #x1C)      ; Accelerometer specific configration register
(defvar MPU6050-REG-INT-PIN-CONFIG #x37)    ; Interrupt pin configuration register
(defvar MPU6050-REG-INT-ENABLE #x38)        ; Interrupt enable configuration register
(defvar MPU6050-REG-INT-STATUS #x3A)        ; Interrupt status register
(defvar MPU6050-REG-WHO-AM-I #x75)          ; Divice ID register
(defvar MPU6050-REG-SIGNAL-PATH-RESET #x68) ; Signal path reset register
(defvar MPU6050-REG-USER-CTRL #x6A)         ; FIFO and I2C Master control register
(defvar MPU6050-REG-PWR-MGMT-1 #x6B)        ; Primary power/sleep control register
(defvar MPU6050-REG-PWR-MGMT-2 #x6C)        ; Secondary power/sleep control register
(defvar MPU6050-REG-TEMP-OUT #x41)          ; Temperature data high byte register
(defvar MPU6050-REG-ACCEL-OUT #x3B)         ; base address for sensor data reads
(defvar MPU6050-REG-MOT-THR #x1F)           ; Motion detection threshold bits [7:0]
(defvar MPU6050-REG-MOT-DUR #x20)           ; Duration counter threshold for motion int. 1 kHz rate, LSB = 1 ms


;; @brief FSYNC output values
;; Allowed values for `setFsyncSampleOutput`.

(defvar MPU6050-FSYNC-OUT-DISABLED 0)
(defvar MPU6050-FSYNC-OUT-TEMP 1)
(defvar MPU6050-FSYNC-OUT-GYROX 2)
(defvar MPU6050-FSYNC-OUT-GYROY 3)
(defvar MPU6050-FSYNC-OUT-GYROZ 4)
(defvar MPU6050-FSYNC-OUT-ACCELX 5)
(defvar MPU6050-FSYNC-OUT-ACCELY 6)
(defvar MPU6050-FSYNC-OUT-ACCEL-Z 7)


;; @brief Clock source options
;; Allowed values for `setClock`.

(defvar MPU6050-INTR-8MHz 0)
(defvar MPU6050-PLL-GYROX 1)
(defvar MPU6050-PLL-GYROY 2)
(defvar MPU6050-PLL-GYROZ 3)
(defvar MPU6050-PLL-EXT-32K 4)
(defvar MPU6050-PLL-EXT-19MHz 5)
(defvar MPU6050-STOP 7)


;; @brief Accelerometer range options
;; Allowed values for `setAccelerometerRange`.

(defvar MPU6050-ACCEL-RANGE-2-G 0)            ; +/- 2g - default
(defvar MPU6050-ACCEL-RANGE-4-G 1)            ; +/- 4g
(defvar MPU6050-ACCEL-RANGE-8-G 2)            ; +/- 8g
(defvar MPU6050-ACCEL-RANGE-16-G 3)           ; +/- 16g


;; @brief Gyroscope range options
;; Allowed values for `setGyroRange`.

(defvar MPU6050-GYRO-RANGE-250-DEG 0)        ; +/- 250 deg/s - default
(defvar MPU6050-GYRO-RANGE-500-DEG 1)        ; +/- 500 deg/s
(defvar MPU6050-GYRO-RANGE-1000-DEG 2)       ; +/- 1000 deg/s
(defvar MPU6050-GYRO-RANGE-2000-DEG 3)       ; +/- 2000 deg/s


;; @brief Digital low pass filter bandthwidth options
;; Allowed values for `setFilterBandwidth`.

(defvar MPU6050-BAND-260-HZ 0)          ; Docs imply this disables the filter
(defvar MPU6050-BAND-184-HZ 1)          ; 184 Hz
(defvar MPU6050-BAND-94-HZ 2)           ; 94 Hz
(defvar MPU6050-BAND-44-HZ 3)           ; 44 Hz
(defvar MPU6050-BAND-21-HZ 4)           ; 21 Hz
(defvar MPU6050-BAND-10-HZ 5)           ; 10 Hz
(defvar MPU6050-BAND-5-HZ 6)            ; 5 Hz


;; @brief Accelerometer high pass filter options
;; Allowed values for `setHighPassFilter`.

(defvar MPU6050-HIGHPASS-DISABLE 0)
(defvar MPU6050-HIGHPASS-5-HZ 1)
(defvar MPU6050-HIGHPASS-2-5-HZ 2)
(defvar MPU6050-HIGHPASS-1-25-HZ 3)
(defvar MPU6050-HIGHPASS-0-63-HZ 4)
(defvar MPU6050-HIGHPASS-UNUSED 5)
(defvar MPU6050-HIGHPASS-HOLD 6)


;; @brief Periodic measurement options
;; Allowed values for `setCycleRate`.

(defvar MPU6050-CYCLE-1-25-HZ 0)        ; 1.25 Hz
(defvar MPU6050-CYCLE-5-HZ 1)           ; 5 Hz
(defvar MPU6050-CYCLE-20-HZ 2)          ; 20 Hz
(defvar MPU6050-CYCLE-40-HZ 3)          ; 40 Hz



(defun mpu6050-init ()
  (mpu6050-reset)
  (mpu6050-filter-bandwidth MPU6050-BAND-260-HZ)
  (mpu6050-gyro-range MPU6050-GYRO-RANGE-500-DEG)
  (mpu6050-accelerometer-range MPU6050-ACCEL-RANGE-2-G)
  (delay 100)
  (mpu6050-clock-source MPU6050-PLL-GYROX)
  (delay 100)
  (mpu6050-sleep nil))


(defun mpu6050-acceleration ())

(defun mpu6050-accelerometer-range ())

(defun mpu6050-clock-source ())

(defun mpu6050-cycle ())

(defun mpu6050-cycle-rate ())

(defun mpu6050-filter-bandwidth (&optional value)
  (let (current)
    (with-i2c (s MPU6050-I2CADDR-DEFAULT)
      (write-byte MPU6050-REG-CONFIG s)
      (restart-i2c s 1)
      (setq current (read-byte s)))
    (when value
      (with-i2c (s MPU6050-I2CADDR-DEFAULT)
        (write-byte MPU6050-REG-CONFIG s)
        (write-byte (logand #xFF (logior (logand current #xF8) (logand value #x07))) s)))
    current))

(defun mpu6050-gyro ())

(defun mpu6050-gyro-range ())

(defun mpu6050-reset ()
  (with-i2c (s MPU6050-I2CADDR-DEFAULT)
    (write-byte MPU6050-REG-PWR-MGMT-1 s)
    (write-byte #x80 s))
  (loop
    (with-i2c (s MPU6050-I2CADDR-DEFAULT)
      (write-byte MPU6050-REG-PWR-MGMT-1 s)
      (restart-i2c s 1)
      (when (zerop (logand (read-byte s) #x80))
        (return)))
    (delay 1))
  (delay 100)
  (with-i2c (s MPU6050-I2CADDR-DEFAULT)
    (write-byte MPU6050-REG-SIGNAL-PATH-RESET s)
    (write-byte 7 s))
  (delay 100))

(defun mpu6050-sample-rate-divisor ())

(defun mpu6050-sleep ())

(defun mpu6050-temperature ()
  (with-i2c (s MPU6050-I2CADDR-DEFAULT)
    (write-byte MPU6050-REG-TEMP-OUT s)
    (restart-i2c s 2)
    (let* ((hi (read-byte s))
           (lo (read-byte s)))
      (+ (/ (float (logior (ash hi 8) (logand lo #xFF))) 340.0) 36.53))))

;;; some testing

(mpu6050-reset)
(mpu6050-filter-bandwidth 5)
(print (mpu6050-filter-bandwidth))
