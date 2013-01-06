(in-package :laac.aac)

(defconstant +WINDOW_LEN_LONG+ 1024)
(defconstant +WINDOW_LEN_SHORT+ (/ +WINDOW_LEN_LONG+ 8))

;; TODO: 設定を見て、こっちも使用するようにする
(defconstant +WINDOW_SMALL_LEN_LONG+ 960)
(defconstant +WINDOW_SMALL_LEN_SHORT+ (/ +WINDOW_SMALL_LEN_LONG+ 8))

(defstruct filterbank
  (prev-window-shape 0 :type (unsigned-byte 1))
  (overlap (make-array +WINDOW_LEN_LONG+ :initial-element 0))
  )

(defun process-filterbank (filterbank window-sequence-name window-shape data)
  (declare (filterbank filterbank))
  (list filterbank window-sequence-name window-shape data)
  )