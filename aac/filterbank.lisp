(in-package :laac.aac)

(defconstant +WINDOW_LEN_LONG+ 1024)
(defconstant +WINDOW_LEN_SHORT+ (/ +WINDOW_LEN_LONG+ 8))

;; TODO: 設定を見て、こっちも使用するようにする
(defconstant +WINDOW_SMALL_LEN_LONG+ 960)
(defconstant +WINDOW_SMALL_LEN_SHORT+ (/ +WINDOW_SMALL_LEN_LONG+ 8))

(defun long-window-ref (window-shape index)
  (ecase window-shape
	(0 (aref *sine-1024* index))
	(1 (aref *kbd-1024* index))))

(defun short-window-ref (window-shape index)
  (ecase window-shape
    (0 (aref *sine-128* index))
	(1 (aref *kbd-128* index))))

(defstruct filterbank
  (prev-window-shape 0 :type (unsigned-byte 1))
  (overlap (make-array +WINDOW_LEN_LONG+ :initial-element 0))
  (mdct-long (new-mdct (* +WINDOW_LEN_LONG+ 2)))
  (mdct-short (new-mdct (* +WINDOW_LEN_SHORT+ 2)))
  )

(defun process-filterbank (filterbank window-sequence-name window-shape data)
  (declare (filterbank filterbank))
  (with-slots (prev-window-shape overlap mdct-long mdct-short) filterbank
    (let* ((buf (make-array (* +WINDOW_LEN_LONG+ 2) :initial-element 0))
		   (length +WINDOW_LEN_LONG+)
		   (short-len +WINDOW_LEN_SHORT+)
		   (mid (floor (- length short-len) 2))
		   (out (make-array length :initial-element 0)))
      (ecase window-sequence-name
        (:only-long-sequence (error "unsupported: ~a" window-sequence-name))

        (:long-start-sequence
         (process-mdct mdct-long data buf 0)
		 
         ;; add second half output of previous frame to windowed output of current frame
         (loop FOR i FROM 0 BELOW length DO
           (setf (aref out i) (+ (aref overlap i) (* (aref buf i) (long-window-ref prev-window-shape i)))))
		 
		 ;; window the second half and save as overlap for next frame
		 (loop FOR i FROM 0 BELOW mid DO
		   (setf (aref overlap i) (aref buf (+ length i))))

		 (loop FOR i FROM 0 BELOW short-len DO
		   (setf (aref overlap (+ mid i)) (* (aref buf (+ length mid i)) (short-window-ref window-shape (- short-len i 1)))))

		 (loop FOR i FROM 0 BELOW mid DO
		   (setf (aref overlap (+ mid short-len i)) 0))
		 )
		
        (:eight-short-sequence (error "unsupported: ~a" window-sequence-name))

        (:long-stop-sequence (error "unsupported: ~a" window-sequence-name)))
    
	  (print `(:out ,out))
	  out)))
