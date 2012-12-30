(in-package :laac.aac)

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

(defun get-num-window-groups (ics-info)
  (declare (ics-info ics-info))
  (case (window-sequence-name (-> ics-info window-sequence))
    ((:only-long-sequence :long-start-sequence :long-stop-sequence)
     1)
    ((:eight-short-sequence)
     (error "TODO: not implemented: window-sequence=~a" :eight-short-sequence)
     )))

(defstruct channel-stream
  (global-gain 0 :type (unsigned-byte 8))
  (ics-info nil :type (or null ics-info))
  (section-data t ) ; TODO
  (scale-factor-data t) ; TODO
  (pulse-data-present 0 :type (unsigned-byte 1))
  (pulse-data t ) ; TODO
  (tns-data-present 0 :type (unsigned-byte 1))
  (tns-data t ) ; TODO
  (gain-control-data-present 0 :type (unsigned-byte 1))
  (gain-control-data t ) ; TODO
  (spectral-data t ) ; TODO
  (length-of-reordered-spectral-data 0 :type (unsigned-byte 14))
  (length-of-longest-codeword 0 :type (unsigned-byte 6))
  (reordered-spectral-data t ) ; TODO
  )

(defstruct channel-pair-element 
  (element-instance-tag 0 :type (unsigned-byte 4))
  (common-window        0 :type (unsigned-byte 1))
  
  ;; present if common-window == 1
  (ics-info           nil :type (or null ics-info))
  (ms-mask-present      0 :type (unsigned-byte 2))
  
  ;; present if ms-mask-present == 1
  (ms-used            nil :type (or null (simple-array (unsigned-byte 1) (* *))))
  
  (channel-stream1 t :type channel-stream)
  (channel-stream2 t :type channel-stream)
  )
