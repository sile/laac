(in-package :laac.aac)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package :laac.bit-stream :laac.bit-stream '(:bit-stream)))

(defun parse-single-channel-element (in)
  (declare (bit-stream:bit-stream in)
           (ignore in))
  :sce)

(defun parse-channel-pair-element (in)
  (declare (bit-stream:bit-stream in))
  
  :cpe)

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

(defun parse-raw-data-block (in)
  (declare (bit-stream:bit-stream in))
  (prog1
      (loop REPEAT 1 ;; XXX: for develop
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
