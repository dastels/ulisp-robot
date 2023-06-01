;;; MPU6050 support

(defvar MPU6050_I2CADDR_DEFAULT #x68)   ; MPU6050 default i2c address w/ AD0 high
(defvar MPU6050_DEVICE_ID #x68)         ; The correct MPU6050_WHO_AM_I value

(defvar MPU6050_REG_SELF_TEST_X #x0D)       ; Self test factory calibrated values register
(defvar MPU6050_REG_SELF_TEST_Y #x0E)       ; Self test factory calibrated values register
(defvar MPU6050_REG_SELF_TEST_Z #x0F)       ; Self test factory calibrated values register
(defvar MPU6050_REG_SELF_TEST_A #x10)       ; Self test factory calibrated values register
(defvar MPU6050_REG_SMPLRT_DIV #x19)        ; sample rate divisor register
(defvar MPU6050_REG_CONFIG #x1A)            ; General configuration register
(defvar MPU6050_REG_GYRO_CONFIG #x1B)       ; Gyro specfic configuration register
(defvar MPU6050_REG_ACCEL_CONFIG #x1C)      ; Accelerometer specific configration register
(defvar MPU6050_REG_INT_PIN_CONFIG #x37)    ; Interrupt pin configuration register
(defvar MPU6050_REG_INT_ENABLE #x38)        ; Interrupt enable configuration register
(defvar MPU6050_REG_INT_STATUS #x3A)        ; Interrupt status register
(defvar MPU6050_REG_WHO_AM_I #x75)          ; Divice ID register
(defvar MPU6050_REG_SIGNAL_PATH_RESET #x68) ; Signal path reset register
(defvar MPU6050_REG_USER_CTRL #x6A)         ; FIFO and I2C Master control register
(defvar MPU6050_REG_PWR_MGMT_1 #x6B)        ; Primary power/sleep control register
(defvar MPU6050_REG_PWR_MGMT_2 #x6C)        ; Secondary power/sleep control register
(defvar MPU6050_REG_TEMP_OUT #x41)          ; Temperature data high byte register
(defvar MPU6050_REG_ACCEL_OUT #x3B)         ; base address for sensor data reads
(defvar MPU6050_REG_MOT_THR #x1F)           ; Motion detection threshold bits [7:0]
(defvar MPU6050_REG_MOT_DUR #x20)           ; Duration counter threshold for motion int. 1 kHz rate, LSB = 1 ms


;; @brief FSYNC output values
;; Allowed values for `setFsyncSampleOutput`.

(defvar MPU6050_FSYNC_OUT_DISABLED 0)
(defvar MPU6050_FSYNC_OUT_TEMP 1)
(defvar MPU6050_FSYNC_OUT_GYROX 2)
(defvar MPU6050_FSYNC_OUT_GYROY 3)
(defvar MPU6050_FSYNC_OUT_GYROZ 4)
(defvar MPU6050_FSYNC_OUT_ACCELX 5)
(defvar MPU6050_FSYNC_OUT_ACCELY 6)
(defvar MPU6050_FSYNC_OUT_ACCEL_Z 7)


;; @brief Clock source options
;; Allowed values for `setClock`.

(defvar MPU6050_INTR_8MHz 0)
(defvar MPU6050_PLL_GYROX 1)
(defvar MPU6050_PLL_GYROY 2)
(defvar MPU6050_PLL_GYROZ 3)
(defvar MPU6050_PLL_EXT_32K 4)
(defvar MPU6050_PLL_EXT_19MHz 5)
(defvar MPU6050_STOP 7)


;; @brief Accelerometer range options
;; Allowed values for `setAccelerometerRange`.

(defvar MPU6050_ACCEL_RANGE_2_G 0)            ; +/- 2g - default
(defvar MPU6050_ACCEL_RANGE_4_G 1)            ; +/- 4g
(defvar MPU6050_ACCEL_RANGE_8_G 2)            ; +/- 8g
(defvar MPU6050_ACCEL_RANGE_16_G 3)           ; +/- 16g


;; @brief Gyroscope range options
;; Allowed values for `setGyroRange`.

(defvar MPU6050_GYRO_RANGE_250_DEG 0)        ; +/- 250 deg/s - default
(defvar MPU6050_GYRO_RANGE_500_DEG 1)        ; +/- 500 deg/s
(defvar MPU6050_GYRO_RANGE_1000_DEG 2)       ; +/- 1000 deg/s
(defvar MPU6050_GYRO_RANGE_2000_DEG 3)       ; +/- 2000 deg/s


;; @brief Digital low pass filter bandthwidth options
;; Allowed values for `setFilterBandwidth`.

(defvar MPU6050_BAND_260_HZ 0)          ; Docs imply this disables the filter
(defvar MPU6050_BAND_184_HZ 1)          ; 184 Hz
(defvar MPU6050_BAND_94_HZ 2)           ; 94 Hz
(defvar MPU6050_BAND_44_HZ 3)           ; 44 Hz
(defvar MPU6050_BAND_21_HZ 4)           ; 21 Hz
(defvar MPU6050_BAND_10_HZ 5)           ; 10 Hz
(defvar MPU6050_BAND_5_HZ 6)            ; 5 Hz


;; @brief Accelerometer high pass filter options
;; Allowed values for `setHighPassFilter`.

(defvar MPU6050_HIGHPASS_DISABLE 0)
(defvar MPU6050_HIGHPASS_5_HZ 1)
(defvar MPU6050_HIGHPASS_2_5_HZ 2)
(defvar MPU6050_HIGHPASS_1_25_HZ 3)
(defvar MPU6050_HIGHPASS_0_63_HZ 4)
(defvar MPU6050_HIGHPASS_UNUSED 5)
(defvar MPU6050_HIGHPASS_HOLD 6)


;; @brief Periodic measurement options
;; Allowed values for `setCycleRate`.

(defvar MPU6050_CYCLE_1_25_HZ 0)        ; 1.25 Hz
(defvar MPU6050_CYCLE_5_HZ 1)           ; 5 Hz
(defvar MPU6050_CYCLE_20_HZ 2)          ; 20 Hz
(defvar MPU6050_CYCLE_40_HZ 3)          ; 40 Hz

;;; cached data: accX accY accZ temp gyroX gyroY gyroZ

(defvar mpu6050-current-accel '(0 0 0))
(defvar mpu6050-current-temp 0)
(defvar mpu6050-current-gyro '(0 0 0))


(defun mpu6050-read ()
  (let ((byte-data '(0 0 0 0 0 0 0 0 0 0 0 0 0 0))
        (raw-data '(0 0 0 0 0 0 0)))
    (dotimes (i 14)
      (with-i2c (s MPU6050_I2CADDR_DEFAULT)
        (write-byte (+ MPU6050_REG_ACCEL_OUT i) s)
        (restart-i2c s 1)
        (let ((byte (read-byte s)))
          (setf (nth i byte-data) byte)
          (format t "~%Byte ~a: ~x~%" i byte))))
    (dotimes (i 7)
      (let ((hi (nth (* i 2) byte-data))
            (lo (nth (1+ (* i 2)) byte-data)))
        (format t "~%Hi: ~a Lo: ~a~%" hi lo)
        (setf (nth i raw-data)  (logior (ash hi 8) (logand lo #xFF)))))
    (print raw-data)
    ;; accel
    (let* ((accel-range (mpu6050-accelerometer-range))
           (accel-scale (cond ((= accel-range MPU6050_ACCEL_RANGE_16_G) 2048)
                              ((= accel-range MPU6050_ACCEL_RANGE_8_G) 4096)
                              ((= accel-range MPU6050_ACCEL_RANGE_4_G) 8192)
                              ((= accel-range MPU6050_ACCEL_RANGE_2_G) 16384)))
           (gyro-range (mpu6050-gyro-range))
           (gyro-scale (cond ((= gyro-range MPU6050_GYRO_RANGE_250_DEG) 131)
                             ((= gyro-range MPU6050_GYRO_RANGE_500_DEG) 65.5)
                             ((= gyro-range MPU6050_GYRO_RANGE_1000_DEG) 32.8)
                             ((= gyro-range MPU6050_GYRO_RANGE_2000_DEG) 16.4))))
      (dotimes (i 3)
        (setf (nth i mpu6050-current-accel) (/ (float (nth i raw-data)) accel-scale)))
      ;; temperature
      (setq mpu6050-current-temp (+ (/ (nth 3 raw-data) 340.0) 36.53))
      ;; gyro
      (dotimes (i 3)
        (setf (nth i mpu6050-current-gyro) (/ (float (nth (+ i 4) raw-data)) gyro-scale))))))


(defun mpu6050-init ()
  (mpu6050-reset)
  (mpu6050-sample-rate-divisor 0)
  (mpu6050-filter-bandwidth MPU6050_BAND_260_HZ)
  (mpu6050-gyro-range MPU6050_GYRO_RANGE_500_DEG)
  (mpu6050-accelerometer-range MPU6050_ACCEL_RANGE_2_G)
  (delay 100)
  (mpu6050-clock-source MPU6050_PLL_GYROX)
  (delay 100)
  (mpu6050-sleep nil))


(defun mpu6050-acceleration ())


(defun mpu6050-accelerometer-range (&optional value)
  (let (current)
    (with-i2c (s MPU6050_I2CADDR_DEFAULT)
      (write-byte MPU6050_REG_ACCEL_CONFIG s)
      (restart-i2c s 1)
      (setq current (read-byte s)))
    (when value
      (with-i2c (s MPU6050_I2CADDR_DEFAULT)
        (write-byte MPU6050_REG_ACCEL_CONFIG s)
        (write-byte (logior (logand current #xE7)  (logand (ash value 3) #x18)) s)))
    (ash (logand current #x18) -3)))


(defun mpu6050-high-pass-filter (&optional value)
  (let (current)
    (with-i2c (s MPU6050_I2CADDR_DEFAULT)
      (write-byte MPU6050_REG_ACCEL_CONFIG s)
      (restart-i2c s 1)
      (setq current (read-byte s)))
    (when value
      (with-i2c (s MPU6050_I2CADDR_DEFAULT)
        (write-byte MPU6050_REG_ACCEL_CONFIG s)
        (write-byte (logior (logand current #xF8)  (logand value #x07)) s)))
    (logand current #x07)))


(defun mpu6050-clock-source (&optional value)
  (let (current)
    (with-i2c (s MPU6050_I2CADDR_DEFAULT)
      (write-byte MPU6050_REG_PWR_MGMT_1 s)
      (restart-i2c s 1)
      (setq current (read-byte s)))
    (when value
      (with-i2c (s MPU6050_I2CADDR_DEFAULT)
        (write-byte MPU6050_REG_PWR_MGMT_1 s)
        (write-byte (logior (logand current #xF8) (logand value #x07)) s)))
    (logand current #x07)))


;; Controls sensor's 'Cycle' measurement mode
;; param:  enable
;;         If `true` the sensor will take measurements at the rate
;;         set by calling `setCycleRate`, sleeping between measurements.
;;         Setting the sensor into 'Cycle' mode will have no effect
;;         if the sensor has been put into a sleep state with `enableSleep`
;;         Setting `false` returns the sensor to the normal
;;         measurement mode.
;; Returns True or false on successful write

(defun mpu6050-cycle (&optional value)
  (let (current)
    (with-i2c (s MPU6050_I2CADDR_DEFAULT)
      (write-byte MPU6050_REG_PWR_MGMT_1 s)
      (restart-i2c s 1)
      (setq current (read-byte s)))
    (when value
      (with-i2c (s MPU6050_I2CADDR_DEFAULT)
        (write-byte MPU6050_REG_PWR_MGMT_1 s)
        (write-byte (logior (logand current #xDF) (logand (ash value 5) #x20)) s)))
    (ash (logand current #x20) -5)))


(defun mpu6050-cycle-rate (&optional value)
  (let (current)
    (with-i2c (s MPU6050_I2CADDR_DEFAULT)
      (write-byte MPU6050_REG_PWR_MGMT_2 s)
      (restart-i2c s 1)
      (setq current (read-byte s)))
    (when value
      (with-i2c (s MPU6050_I2CADDR_DEFAULT)
        (write-byte MPU6050_REG_PWR_MGMT_2 s)
        (write-byte (logior (logand current #x3F) (logand (ash value 6) #xC0)) s)))
    (ash (logand current #xC0) -6)))


(defun mpt6050-fsync-sample-output (&optional value)
  (let (current)
    (with-i2c (s MPU6050_I2CADDR_DEFAULT)
      (write-byte MPU6050_REG_CONFIG s)
      (restart-i2c s 1)
      (setq current (read-byte s)))
    (when value
      (with-i2c (s MPU6050_I2CADDR_DEFAULT)
        (write-byte MPU6050_REG_CONFIG s)
        (write-byte (logior (logand current #xC7) (logand (ash value 3) #x38)) s)))
    (ash (logand current #x38) -3)))


(defun mpu6050-filter-bandwidth (&optional value)
  (let (current)
    (with-i2c (s MPU6050_I2CADDR_DEFAULT)
      (write-byte MPU6050_REG_CONFIG s)
      (restart-i2c s 1)
      (setq current (read-byte s)))
    (when value
      (with-i2c (s MPU6050_I2CADDR_DEFAULT)
        (write-byte MPU6050_REG_CONFIG s)
        (write-byte (logior (logand current #xF8) (logand value #x07)) s)))
    (logand current #x07)))


(defun mpu6050-gyro ())


(defun mpu6050-gyro-range (&optional value)
    (let (current)
    (with-i2c (s MPU6050_I2CADDR_DEFAULT)
      (write-byte MPU6050_REG_GYRO_CONFIG s)
      (restart-i2c s 1)
      (setq current (read-byte s)))
    (when value
      (with-i2c (s MPU6050_I2CADDR_DEFAULT)
        (write-byte MPU6050_REG_GYRO_CONFIG s)
        (write-byte (logior (logand current #xE7)  (logand (ash value 3) #x18)) s)))
    (logand (ash current -3) #x03)))


(defun mpu6050-reset ()
  ;; turn on the reset bit
  (let (current)
    (with-i2c (s MPU6050_I2CADDR_DEFAULT)
      (write-byte MPU6050_REG_PWR_MGMT_1 s)
      (restart-i2c s 1)
      (setq current (read-byte s)))
    (with-i2c (s MPU6050_I2CADDR_DEFAULT)
      (write-byte MPU6050_REG_PWR_MGMT_1 s)
      (write-byte (logior current #x80) s)))
  ;; wait for reset to complete
  (loop
    (with-i2c (s MPU6050_I2CADDR_DEFAULT)
      (write-byte MPU6050_REG_PWR_MGMT_1 s)
      (restart-i2c s 1)
      (when (zerop (logand (read-byte s) #x80))
        (return)))
    (delay 1))
  (delay 100)
  (with-i2c (s MPU6050_I2CADDR_DEFAULT)
    (write-byte MPU6050_REG_SIGNAL_PATH_RESET s)
    (write-byte #x07 s))
  (delay 100))


(defun mpu6050-sample-rate-divisor (&optional value)
  (if value
      (with-i2c (s MPU6050_I2CADDR_DEFAULT)
        (write-byte MPU6050_REG_SMPLRT_DIV s)
        (write-byte value s))
      (with-i2c (s MPU6050_I2CADDR_DEFAULT)
        (write-byte MPU6050_REG_SMPLRT_DIV s)
        (restart-i2c s 1)
        (read-byte s))))


(defun mpu6050-sleep (&optional value)
  (let (current)
    (with-i2c (s MPU6050_I2CADDR_DEFAULT)
      (write-byte MPU6050_REG_PWR_MGMT_1 s)
      (restart-i2c s 1)
      (setq current (read-byte s)))
    (when value
      (with-i2c (s MPU6050_I2CADDR_DEFAULT)
        (write-byte MPU6050_REG_PWR_MGMT_1 s)
        (write-byte (logior logand current #xBF (logand (ash value 6) #x40)) s)))
    (not (zerop (logand (ash (logand current #x40) -6))))))


(defun mpu6050-temperature ()
  (with-i2c (s MPU6050_I2CADDR_DEFAULT)
    (write-byte MPU6050_REG_TEMP_OUT s)
    (restart-i2c s 2)
    (let* ((hi (read-byte s))
           (lo (read-byte s)))
      (+ (/ (float (logior (ash hi 8) (logand lo #xFF))) 340.0) 36.53))))
