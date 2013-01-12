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
  (prev-window-shape 0 :type (unsigned-byte 1)) ;; XXX: ics-infoで管理する?
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
           (trans (/ short-len 2))
           (out (make-array length :initial-element 0)))
      (ecase window-sequence-name
        (:only-long-sequence 
         (process-mdct mdct-long data buf 0)
         
         ;; add second half output of previous frame to windowed output of current frame
         (loop FOR i FROM 0 BELOW length DO
           (setf (aref out i) (+ (aref overlap i)
                                 (* (aref buf i) (long-window-ref prev-window-shape i)))))

         ;; window the second half and save as overlap for next frame
         (loop FOR i FROM 0 BELOW length DO
           (setf (aref overlap i) (* (aref buf (+ length i))
                                     (long-window-ref window-shape (- length 1 i)))))
         )

        (:long-start-sequence
         (process-mdct mdct-long data buf 0)
		 
         ;; add second half output of previous frame to windowed output of current frame
         (loop FOR i FROM 0 BELOW length DO
           (setf (aref out i) (+ (aref overlap i) (* (aref buf i) (long-window-ref prev-window-shape i)))))
		 
         ;; window the second half and save as overlap for next frame
         (loop FOR i FROM 0 BELOW mid DO
           (setf (aref overlap i) (aref buf (+ length i))))
         
         (loop FOR i FROM 0 BELOW short-len DO
           (setf (aref overlap (+ mid i)) (* (aref buf (+ length mid i))
                                             (short-window-ref window-shape (- short-len i 1)))))
         
         (loop FOR i FROM 0 BELOW mid DO
           (setf (aref overlap (+ mid short-len i)) 0))
         )
		
        (:eight-short-sequence 
         (loop FOR i FROM 0 BELOW 8 DO
           (process-mdct mdct-short (nthcdr (* i short-len) data) buf (* 2 i short-len))
           #+C
           (print (list `(:loop ,i)
                        (subseq data (* i short-len) (* (1+ i) short-len))
                        (subseq buf (* 2 i short-len) (* 2 (+ i 1) short-len))))
           )

         ;; add second half output of previous frame to windowed output of current frame
         (loop FOR i FROM 0 BELOW mid DO
           (setf (aref out i) (aref overlap i)))
         (loop FOR i FROM 0 BELOW short-len DO
           (macrolet ((out-set (index value)
                        `(setf (aref out (+ mid (* ,index short-len) i)) ,value))
                      (overlap-ref (index)
                        `(aref overlap (+ mid (* ,index short-len) i)))
                      (buf-ref (index)
                        `(aref buf (+ (* ,index short-len) i))))
             (let ((prev-win (short-window-ref prev-window-shape i))
                   (curr-win-1 (short-window-ref window-shape (- short-len 1 i)))
                   (curr-win-2 (short-window-ref window-shape i)))
               (out-set 0 (+ (overlap-ref 0) (* (buf-ref 0) prev-win)))
               (out-set 1 (+ (overlap-ref 1) (* (buf-ref 1) curr-win-1) (* (buf-ref 2) curr-win-2)))
               (out-set 2 (+ (overlap-ref 2) (* (buf-ref 3) curr-win-1) (* (buf-ref 4) curr-win-2)))
               (out-set 3 (+ (overlap-ref 3) (* (buf-ref 5) curr-win-1) (* (buf-ref 6) curr-win-2)))
               (when (< i trans)
                 (out-set 4 (+ (overlap-ref 4) (* (buf-ref 7) curr-win-1) (* (buf-ref 8) curr-win-2))))
               )))

         ;; window the second half and save as overlap for next frame
         (loop FOR i FROM 0 BELOW short-len DO
           (macrolet ((overlap-set (index value)
                        `(setf (aref overlap (+ mid (* ,index short-len) i (- length))) ,value))
                      (buf-ref (index)
                        `(aref buf (+ i (* short-len ,index)))))
             (let ((curr-win-1 (short-window-ref window-shape (- short-len 1 i)))
                   (curr-win-2 (short-window-ref window-shape i)))
               (when (>= i trans)
                 (overlap-set 4 (+ (* (buf-ref 7) curr-win-1) (* (buf-ref 8) curr-win-2))))
               (overlap-set 5 (+ (* (buf-ref  9) curr-win-1) (* (buf-ref 10) curr-win-2)))
               (overlap-set 6 (+ (* (buf-ref 11) curr-win-1) (* (buf-ref 12) curr-win-2)))
               (overlap-set 7 (+ (* (buf-ref 13) curr-win-1) (* (buf-ref 14) curr-win-2)))
               (overlap-set 8 (+ (* (buf-ref 15) curr-win-1))))))
         
         (loop FOR i FROM 0 BELOW mid DO
           (setf (aref overlap (+ mid short-len i)) 0))
         )

        (:long-stop-sequence
         ;;(print `(:overlaop ,overlap))
         (process-mdct mdct-long data buf 0)
         
         ;; add second half output of previous frame to windowed output of current frame
         ;; construct first half window using padding with 1's and 0's
         (loop FOR i FROM 0 BELOW mid DO
           (setf (aref out i) (aref overlap i)))
        
         (loop FOR i FROM 0 BELOW short-len DO
           (setf (aref out (+ mid i)) (+ (aref overlap (+ mid i)) 
                                         (* (aref buf (+ mid i)) (short-window-ref prev-window-shape i)))))
         
         (loop FOR i FROM 0 BELOW mid DO
           (setf (aref out (+ mid short-len i)) (+ (aref overlap (+ mid short-len i))
                                                   (aref buf (+ mid short-len i)))))
         
         ;; window the second half and save as overlap for next frame
         (loop FOR i FROM 0 BELOW length DO
           (setf (aref overlap i) (* (aref buf (+ length i)) (long-window-ref window-shape (- length 1 i)))))
         
         ;;(print `(:long-stop-sequence ,(length out) ,out))
         ;;(error "stop")
         ))
      
      (setf prev-window-shape window-shape) ; XXX; これで正しい?
      
      out)))
