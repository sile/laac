(in-package :laac.aac)

(defconstant +WINDOW_LEN_LONG+ 1024)
(defconstant +WINDOW_LEN_SHORT+ (/ +WINDOW_LEN_LONG+ 8))

;; TODO: 設定を見て、こっちも使用するようにする
(defconstant +WINDOW_SMALL_LEN_LONG+ 960)
(defconstant +WINDOW_SMALL_LEN_SHORT+ (/ +WINDOW_SMALL_LEN_LONG+ 8))

(defstruct filterbank
  (prev-window-shape 0 :type (unsigned-byte 1))
  (overlap (make-array +WINDOW_LEN_LONG+ :initial-element 0))
  (mdct-long (new-mdct (* +WINDOW_LEN_LONG+ 2)))
  (mdct-short (new-mdct (* +WINDOW_LEN_SHORT+ 2)))
  )

(defun process-filterbank (filterbank window-sequence-name window-shape data)
  (declare (filterbank filterbank))
  (ecase window-sequence-name
    (:only-long-sequence (error "unsupported: ~a" window-sequence-name))
    (:long-start-sequence 
     )
    (:eight-short-sequence (error "unsupported: ~a" window-sequence-name))
    (:long-stop-sequence (error "unsupported: ~a" window-sequence-name)))
  (list filterbank window-sequence-name window-shape data)
  )
