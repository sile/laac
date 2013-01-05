(in-package :laac.aac)

;; XXX: configとかに持っていく
(defparameter *sampling-frequency-index* 0)

(defconstant +ZERO_HCB+ 0)
(defconstant +FIRST_PAIR_HCB+ 5)
(defconstant +ESC_HCB+ 11)
(defconstant +QUAD_LEN+ 4)
(defconstant +PAIR_LEN+ 2)
(defconstant +NOISE_HCB+ 13)
(defconstant +INTENSITY_HCB2+ 14)
(defconstant +INTENSITY_HCB+ 15)
(defconstant +ESC_FLAG+ 16)

(defun syntactic-element-name (id)
  (declare ((integer 0 7) id))
  (case id
    (0 :sce)
    (1 :cpe)
    (2 :cce)
    (3 :lfe)
    (4 :dse)
    (5 :pce)
    (6 :fil)
    (7 :end)))

;; table 4.109 window sequences
(defun window-sequence-name (value)
  (declare ((integer 0 3) value))
  (case value
    (0 :only-long-sequence)
    (1 :long-start-sequence)
    (2 :eight-short-sequence)
    (3 :long-stop-sequence)))

(defstruct ics-info
  (ics-reserved-bit 0 :type (unsigned-byte 1))
  (window-sequence 0 :type (unsigned-byte 2))
  (window-shape 0 :type (unsigned-byte 1))
  (max-sfb 0 :type (unsigned-byte 6))
  
  ;; present if window-sequence == EIGHT_SHORT_SEQUENCE
  (scale-factor-grouping 0 :type (unsigned-byte 7))
  
  ;; present if window-sequence != EIGHT_SHORT_SEQUENCE
  (predictor-data-present 0 :type (unsigned-byte 1))
  
  ;; present if predictor-data-present == 1
  ;;  present if audioObjectType == 1 (lc)
  (predictor-reset 0 :type (unsigned-byte 1))
  (predictor-reset-group-number 0 :type (unsigned-byte 5)) ;; present if predictor-reset == 1
  (prediction-used nil :type (or null (vector (unsigned-byte 1))))
  )

;; 4.5.2.3.4 Scalefactor bands and grouping
(defun get-num-window-groups (ics-info)
  (declare (ics-info ics-info))
  (ecase (window-sequence-name (-> ics-info window-sequence))
    ((:only-long-sequence :long-start-sequence :long-stop-sequence)
     1)
    ((:eight-short-sequence)
         (let ((num-window-groups 1)
                   (num-windows 8))
           (loop FOR i FROM 0 BELOW (1- num-windows) 
                 UNLESS (ldb-test (byte 1 (- 6 i)) (-> ics-info scale-factor-grouping))
                 DO (incf num-window-groups))
           num-window-groups
           )
     )))

;; 4.5.2.3.4 Scalefactor bands and grouping
(defun get-window-group-length (ics-info)
  (declare (ics-info ics-info))
  (let* ((num-window-groups (get-num-window-groups ics-info))
         (window-group-length (make-array num-window-groups :element-type 'fixnum :initial-element 0))
         (num-windows 8))
    (incf (aref window-group-length 0))

    (ecase (window-sequence-name (-> ics-info window-sequence))
      ((:only-long-sequence :long-start-sequence :long-stop-sequence)
       (setf (aref window-group-length 0) 1))
      ((:eight-short-sequence)
       (loop WITH group = 1
             FOR i FROM 0 BELOW (1- num-windows)
             DO
             (unless (ldb-test (byte 1 (- 6 i)) (-> ics-info scale-factor-grouping))
               (incf group))
             (incf (aref window-group-length (1- group))))))
    window-group-length))

;; 4.5.2.3.4 Scalefactor bands and grouping
(defun get-sect-sfb-offset (ics-info)
  (declare (ics-info ics-info))
  (let* ((swb-offset-long-window (get-swb-offset-long-window *sampling-frequency-index*))
         (swb-offset-short-window (get-swb-offset-short-window *sampling-frequency-index*))
         (window-group-length (get-window-group-length ics-info))
         (num-window-groups (get-num-window-groups ics-info))
         (sect-sfb-offset (make-array `(,num-window-groups ,(1+ (-> ics-info max-sfb))) :element-type 'fixnum)))
    (ecase (window-sequence-name (-> ics-info window-sequence))
      ((:only-long-sequence :long-start-sequence :long-stop-sequence)
       (loop FOR i FROM 0 TO (-> ics-info max-sfb) 
             DO (setf (aref sect-sfb-offset 0 i) (aref swb-offset-long-window i))))
      ((:eight-short-sequence)
       (loop FOR g FROM 0 BELOW num-window-groups DO
             (loop WITH offset = 0
                   FOR sect-sfb FROM 0 BELOW (-> ics-info max-sfb) 
                   FOR i = sect-sfb
               DO
               (let ((width (* (- (aref swb-offset-short-window (1+ i))
                                  (aref swb-offset-short-window i))
                               (aref window-group-length g))))
                 (setf (aref sect-sfb-offset g sect-sfb) offset)
                 (incf offset width))
               FINALLY
               (setf (aref sect-sfb-offset g sect-sfb) offset)))))
    sect-sfb-offset))

;; 4.5.2.3.4 Scalefactor bands and grouping
(defun get-swb-offset (ics-info)
  (declare (ics-info ics-info))
  (let* ((swb-offset-long-window (get-swb-offset-long-window *sampling-frequency-index*))
         (swb-offset-short-window (get-swb-offset-short-window *sampling-frequency-index*)))
    (ecase (window-sequence-name (-> ics-info window-sequence))
      ((:only-long-sequence :long-start-sequence :long-stop-sequence)
       (let ((swb-offset (make-array (1+ (-> ics-info max-sfb)) :element-type 'fixnum)))
         (dotimes (i (length swb-offset))
           (setf (aref swb-offset i) (aref swb-offset-long-window i)))
         swb-offset))
      ((:eight-short-sequence)
       (let* ((num-swb+1 (length swb-offset-short-window))
              (swb-offset (make-array num-swb+1 :element-type 'fixnum)))
         (dotimes (i (length swb-offset))
           (setf (aref swb-offset i) (aref swb-offset-short-window i)))
         swb-offset))
       )))
         
(defstruct section-data
  (sect-cb      t :type (vector (unsigned-byte 5)))
  (sect-start   t :type (vector fixnum))
  (sect-end     t :type (vector fixnum))
  (sfb-cb       t :type (vector (unsigned-byte 5)))
  (num-sec      t :type (vector fixnum)))

(defstruct scale-factor-data
  (dpcm-is-position t :type (array fixnum (* *)))
  (dpcm-noise-nrg t :type (array fixnum (* *)))
  (dpcm-sf t :type (array fixnum (* *))))

(defstruct gain-control-data
  (max-band 0 :type (unsigned-byte 2))
  (alevcode 0 :type (vector (unsigned-byte 4)))
  (aloccode 0 :type (vector (unsigned-byte 5)))
  (adjust-num 0 :type (vector (unsigned-byte 3)))
  )

(defstruct spectral-data
  data
  )

;; TODO: ics?
(defstruct channel-stream
  (global-gain 0 :type (unsigned-byte 8))
  (ics-info nil :type (or null ics-info))
  (section-data t :type section-data) 
  (scale-factor-data t :type scale-factor-data)
  (pulse-data-present 0 :type (unsigned-byte 1))
  (pulse-data t ) ; TODO
  (tns-data-present 0 :type (unsigned-byte 1))
  (tns-data t ) ; TODO
  (gain-control-data-present 0 :type (unsigned-byte 1))
  (gain-control-data nil :type (or null gain-control-data))
  (spectral-data nil :type (or null spectral-data))
  (length-of-reordered-spectral-data 0 :type (unsigned-byte 14))
  (length-of-longest-codeword 0 :type (unsigned-byte 6))
  (reordered-spectral-data t ) ; TODO
  )

;; TODO: cpe?
(defstruct channel-pair-element 
  (element-instance-tag 0 :type (unsigned-byte 4))
  (common-window        0 :type (unsigned-byte 1))
  
  ;; present if common-window == 1
  (ics-info           nil :type (or null ics-info))
  (ms-mask-present      0 :type (unsigned-byte 2))
  
  ;; present if ms-mask-present == 1
  (ms-used            nil :type (or null (simple-array (unsigned-byte 1) (* *))))
  
  (channel-stream1 t :type channel-stream) ;; TODO: rename left
  (channel-stream2 t :type channel-stream) ;; TODO: rename right
  )
