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

(defstruct ics-info
  )

(defstruct channel-pair-element 
  (element-instance-tag 0 :type (unsigned-byte 4))
  (common-window        0 :type (unsigned-byte 1))
  
  ;; present if common-window == 1
  (ics-info           nil :type (or null ics-info))
  (ms-mask-present      0 :type (unsigned-byte 2))
  
  ;; present if ms-mask-present == 1
  (ms-used            nil :type (or null (simple-array t (* *))))
  
  (channel-stream1 t) ;; TODO:
  (channel-stream2 t) ;; TODO:
  )
