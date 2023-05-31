;;; Library for I2C LC709203F battery status and fuel gauge
;;; Dave Astels

(defvar LC709203F_I2CADDR_DEFAULT #x0B)     ; LC709203F default i2c address

;; Registers

(defvar LC709203F_CMD_THERMISTORB #x06)     ; Read/write thermistor B
(defvar LC709203F_CMD_INITRSOC #x07)        ; Initialize RSOC calculation
(defvar LC709203F_CMD_CELL_TEMPERATURE #x08) ; Read/write batt temperature
(defvar LC709203F_CMD_CELL_VOLTAGE #x09)     ; Read batt voltage
(defvar LC709203F_CMD_APA #x0B)             ; Adjustment Pack Application
(defvar LC709203F_CMD_RSOC #x0D)            ; Read state of charge
(defvar LC709203F_CMD_CELL_ITE #x0F)         ; Read batt indicator to empty
(defvar LC709203F_CMD_IC_VERSION #x11)       ; Read IC version
(defvar LC709203F_CMD_BATT_PROFILE #x12)        ; Set the battery profile
(defvar LC709203F_CMD_ALARM_PERCENTAGE #x13)    ; Alarm on percent threshold
(defvar LC709203F_CMD_ALARM_VOLTAGE #x14)       ; Alarm on voltage threshold
(defvar LC709203F_CMD_POWER_MODE #x15)       ; Sets sleep/power mode
(defvar LC709203F_CMD_STATUSBIT #x16)       ; Temperature obtaining method
(defvar LC709203F_CMD_PARAMETER #x1A)       ; Batt profile code

;; Temperature mode

(defvar LC709203F_TEMPERATURE_I2C #x0000)
(defvar LC709203F_TEMPERATURE_THERMISTOR #x0001)

;;  Chip power state

(defvar LC709203F_POWER_MODE_OPERATE #x0001)
(defvar LC709203F_POWER_MODE_SLEEP #x0002)

;; Approx battery pack size

(defvar LC709203F_APA_100MAH #x08)
(defvar LC709203F_APA_200MAH #x0B)
(defvar LC709203F_APA_400MAH #x0E)
(defvar LC709203F_APA_500MAH #x10)
(defvar LC709203F_APA_1000MAH #x19)
(defvar LC709203F_APA_2000MAH #x2D)
(defvar LC709203F_APA_2200MAH #x30)
(defvar LC709203F_APA_3000MAH #x36)

(defun lc709203-init ()
  (lc709203-power-mode LC709203F_POWER_MODE_OPERATE)
  (lc709203-pack-size LC709203F_APA_500MAH)
  (lc709203-battery-profile 1)
  (delay 100)
  (lc709203-init-RSOC)
  (delay 100))

(defun lc709203-init-RSOC ()
  (with-i2c (s LC709203F_I2CADDR_DEFAULT)
    (write-byte LC709203F_CMD_INITRSOC s)
    (write-word #xAA55 s)))

(defun lc709203-cell-voltage ()
  (with-i2c (s LC709203F_I2CADDR_DEFAULT)
    (write-byte LC709203F_CMD_CELL_VOLTAGE s)
    (restart-i2c s 2)
    (/ (read-word s) 1000.0)))

(defun lc709203-cell-percentage ()
  (with-i2c (s LC709203F_I2CADDR_DEFAULT)
    (write-byte LC709203F_CMD_CELL_ITE s)
    (restart-i2c s 2)
    (/ (read-word s) 10.0)))

(defun lc709203-cell-temperature ()
  (with-i2c (s LC709203F_I2CADDR_DEFAULT)
    (write-byte LC709203F_CMD_CELL_TEMPERATURE s)
    (restart-i2c s 2)
    (- (/ (read-word s) 10.0)) 273.15))

(defun lc709203-version ()
  (with-i2c (s LC709203F_I2CADDR_DEFAULT)
    (write-byte LC709203F_CMD_IC_VERSION s)
    (restart-i2c s 2)
    (read-word s)))

(defun lc709203-power-mode (&optional value)
  (if value
      (with-i2c (s LC709203F_I2CADDR_DEFAULT)
        (write-byte LC709203F_CMD_POWER_MODE s)
        (write-word value s))
      (with-i2c (s LC709203F_I2CADDR_DEFAULT)
        (write-byte LC709203F_CMD_POWER_MODE s)
        (restart-i2c s 2)
        (read-word s))))

(defun lc709203-battery-profile (&optional value)
    (if value
        (with-i2c (s LC709203F_I2CADDR_DEFAULT)
          (write-byte LC709203F_CMD_BATT_PROFILE s)
          (write-word value s))
        (with-i2c (s LC709203F_I2CADDR_DEFAULT)
          (write-byte LC709203F_CMD_BATT_PROFILE s)
          (restart-i2c s 2)
          (read-word s))))

(defun lc709203-pack-size (&optional value)
    (if value
        (with-i2c (s LC709203F_I2CADDR_DEFAULT)
          (write-byte LC709203F_CMD_APA s)
          (write-word value s))
        (with-i2c (s LC709203F_I2CADDR_DEFAULT)
          (write-byte LC709203F_CMD_APA s)
          (restart-i2c s 2)
          (read-word s))))

(defun lc709203-bconstant (&optional value)
    (if value
        (with-i2c (s LC709203F_I2CADDR_DEFAULT)
          (write-byte LC709203F_CMD_THERMISTORB s)
          (write-word value s))
        (with-i2c (s LC709203F_I2CADDR_DEFAULT)
          (write-byte LC709203F_CMD_THERMISTORB s)
          (restart-i2c s 2)
          (read-word s))))

(defun lc709203-thermistor-enable (&optional (value 0))
  (if (not (equal value 0))
      (with-i2c (s LC709203F_I2CADDR_DEFAULT)
        (write-byte LC709203F_CMD_STATUSBIT s)
        (write-word (if value 1 0) s))
      (with-i2c (s LC709203F_I2CADDR_DEFAULT)
        (write-byte LC709203F_CMD_STATUSBIT s)
        (restart-i2c s 2)
        (not (zerop (read-word s))))))

(defun lc709203-low-voltage-alarm-percent (&optional value)
  (if value
      (if (or (< value 0) (> value 100))
          (error "Bad voltage alarm threshold: ~a" value)
          (with-i2c (s LC709203F_I2CADDR_DEFAULT)
            (write-byte LC709203F_CMD_ALARM_PERCENTAGE s)
            (write-word value s)))
      (with-i2c (s LC709203F_I2CADDR_DEFAULT)
        (write-byte LC709203F_CMD_ALARM_PERCENTAGE s)
        (restart-i2c s 2)
        (read-word s))))

(defun lc709203-low-voltage-alarm-level (&optional value)
  (if value
        (with-i2c (s LC709203F_I2CADDR_DEFAULT)
          (write-byte LC709203F_CMD_ALARM_VOLTAGE s)
          (write-word value s))
        (with-i2c (s LC709203F_I2CADDR_DEFAULT)
          (write-byte LC709203F_CMD_ALARM_VOLTAGE s)
          (restart-i2c s 2)
          (read-word s))))

(defun lc709203-generate-crc ()
  )
