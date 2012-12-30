(in-package :laac.aac)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package :laac.bit-stream :laac.bit-stream '(:bit-stream)))

;; XXX:
(defparameter *audio-object-type* :undefined) ;; (member :lc :main :ssr :ltp :undefined)

;; XXX: この値はどこで定義されている？
(defconstant +PRED_SFB_MAX+ most-positive-fixnum)

(defun parse-ics-info (in common-window)
  (declare (bit-stream:bit-stream in)
           (ignore common-window))
  (with-oop-like (in :bit-stream)
    (let ((ics-reserved-bit (in read-bits 1))
          (window-sequence (in read-bits 2))
          (window-shape (in read-bits 1)))
      (if (eq (window-sequence-name window-sequence) :eight-short-sequence)
          (make-ics-info :ics-reserved-bit ics-reserved-bit
                         :window-sequence window-sequence
                         :window-shape window-shape
                         :max-sfb (in read-bits 4)
                         :scale-factor-grouping (in read-bits 7))
        (let ((max-sfb (in read-bits 6))
              (predictor-data-present (in read-bits 1))
              (predictor-reset 0)
              (predictor-reset-group-number 0)
              (prediction-used nil))
          (when (= 1 predictor-data-present)
            (assert (eq :lc *audio-object-type*) () "Unsupported AudioObjectType: ~a" *audio-object-type*)
            (setf predictor-reset (in read-bits 1))
            (when (= 1 predictor-reset)
              (setf predictor-reset-group-number (in read-bits 5)
                    prediction-used (make-array (min max-sfb +PRED_SFB_MAX+)))
              (dotimes (i (length prediction-used))
                (setf (aref prediction-used i) (in read-bits 1))))
            )

          (make-ics-info :ics-reserved-bit ics-reserved-bit
                         :window-sequence window-sequence
                         :window-shape window-shape
                         :max-sfb max-sfb
                         :predictor-data-present predictor-data-present
                         :predictor-reset predictor-reset
                         :predictor-reset-group-number predictor-reset-group-number
                         :prediction-used prediction-used)
          )))))

(defun parse-individual-channel-stream (in common-window scale-flag)
  (declare (bit-stream:bit-stream in)
           ((unsigned-byte 1) common-window scale-flag))
  (with-oop-like (in :bit-stream)
    (let ((global-gain (in read-bits 8))
          (ics-info nil))
      (when (and (= common-window 0) (= scale-flag 0))
        (setf ics-info (parse-ics-info in common-window)))
      
      (make-channel-stream :global-gain global-gain
                           :ics-info ics-info
        )
      )))


(defun parse-single-channel-element (in)
  (declare (bit-stream:bit-stream in)
           (ignore in))
  :sce)

(defun parse-channel-pair-element (in)
  (declare (bit-stream:bit-stream in))
  (with-oop-like (in :bit-stream)
    (let ((element-instance-tag (in read-bits 4))
          (common-window        (in read-bits 1))
          (ics-info nil)
          (ms-mask-present 0)
          (ms-used nil))
      (when (= common-window 1)
        (setf ics-info        (parse-ics-info in common-window)
              ms-mask-present (in read-bits 2))
        (when (= ms-mask-present 1)
          (let ((num-window-groups (get-num-window-groups ics-info))
                (max-sfb (-> ics-info max-sfb)))
            (setf ms-used (make-array `(,num-window-groups ,max-sfb) :element-type '(unsigned-byte 1)))
            (dotimes (g num-window-groups)
              (dotimes (sfb max-sfb)
                (setf (aref ms-used g sfb) (in read-bits 1)))))
          ))
      (make-channel-pair-element
       :element-instance-tag element-instance-tag
       :common-window        common-window
       :ics-info             ics-info
       :ms-mask-present      ms-mask-present
       :ms-used              ms-used
       :channel-stream1 (parse-individual-channel-stream in common-window 0)
       :channel-stream2 (parse-individual-channel-stream in common-window 0)
       ))))

(defun parse-coupling-channel-element (in)
  (declare (bit-stream:bit-stream in) (ignore in))
  :cce)

(defun parse-program-config-element (in)
  (declare (bit-stream:bit-stream in) (ignore in))
  :pce)

(defun parse-lfe-channel-element (in)
  (declare (bit-stream:bit-stream in) (ignore in))
  :lfe)

(defun parse-fill-element (in)
  (declare (bit-stream:bit-stream in) (ignore in))
  :fil)

(defun parse-raw-data-block (in &key (profile :undefined))
  (declare (bit-stream:bit-stream in))
  (prog1
      (loop WITH *audio-object-type* = profile
            REPEAT 1 ;; XXX: for develop
            FOR id-syn-ele = (syntactic-element-name (bit-stream:read-bits in 3))
            UNTIL (eq id-syn-ele :end)
        COLLECT
        (case id-syn-ele
          ((:dse) (error "TODO: unsupported id-syn-ele: ~a" id-syn-ele))
          (:sce (parse-single-channel-element in))
          (:cpe (parse-channel-pair-element in))
          (:cce (parse-coupling-channel-element in))
          (:lfe (parse-lfe-channel-element in))
          (:pce (parse-program-config-element in))
          (:fil (parse-fill-element in))
          ))
    (bit-stream:drop-unaligned-bits in)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package :laac.bit-stream :laac.bit-stream '()))
