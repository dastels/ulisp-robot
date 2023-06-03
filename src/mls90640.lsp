;;; I2C Driver for MLX90640 24x32 IR Thermal Camera
;;; Dave Astels

(defvar MLX90640_I2CADDR_DEFAULT #x33)  ; I2C address by default

(defvar MLX90640_DEVICEID1 #x2407)      ; I2C identification register

;;; Mode to read pixel frames - two per image

(defvar MLX90640_INTERLEAVED 0)         ; Read data from camera by interleaved lines
(defvar MLX90640_CHESS 1)               ; Read data from camera in alternating pixels


;;; Internal ADC resolution for pixel calculation

(defvar MLX90640_ADC_16BIT 0)
(defvar MLX90640_ADC_17BIT 1)
(defvar MLX90640_ADC_18BIT 2)
(defvar MLX90640_ADC_19BIT 3)


;;; How many PAGES we will read per second - 2 pages per frame

(defvar MLX90640_0_5_HZ 0)
(defvar MLX90640_1_HZ 1)
(defvar MLX90640_2_HZ 2)
(defvar MLX90640_4_HZ 3)
(defvar MLX90640_8_HZ 4)
(defvar MLX90640_16_HZ 5)
(defvar MLX90640_32_HZ 6)
(defvar MLX90640_64_HZ 7)


(defvar OPENAIR_TA_SHIFT 8)             ; Default 8 degree offset from ambient air


(defun mlx90640-read ()
  )

(defun mlx90640-write ()
  )


;; Read nMemAddressRead words from I2C startAddress into data
;; param:  start-address I2C memory address to start reading
;; param:  read-count 16-bit words to read
;; return: data read or nil

(defun mlx90640-read (start-address read-count)
  )


(defun mlx90640-write-16 (address data)
  )


(defun mlx90640-mode (&optional value)
  )


(defun mlx90640-resolution (&optional value)
  )


(defun mlx90640-refresh-rate (&optional value)
  )


(defune mlx90640-get-frame ())
