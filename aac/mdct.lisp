(in-package :laac.aac)

(defstruct mdct
  (length 0 :type fixnum)
  (fft t :type fft)
  (buffer t :type (simple-array t (* *)))
  (sincos t :type (simple-array t (* 2)))
  )

(defun new-mdct (length)
  (declare (fixnum length))
  (let ((length/4 (ash length -2)))
    (make-mdct :length length
               :fft (new-fft length/4)
               :buffer (make-array `(,length/4 ,2) :initial-element 0)
               :sincos (ecase length
                         (2048 *mdct-table-2048*)
                         (256  *mdct-table-256*)
                         (1920 *mdct-table-1920*)
                         (240  *mdct-table-240*))
               )))

(defun process-mdct (mdct data out out-off)
  (declare (mdct mdct))
  (let* ((sincos (-> mdct sincos))
         (N (-> mdct length))
         (N2 (ash N -1))
         (N4 (ash N -2))
         (N8 (ash N -4))
         (buf (make-array `(,N4 2) :initial-element 0)))

    ;; pre-IFFT complex multiplication 
    (loop FOR k FROM 0 BELOW N4
          FOR in1 = (or (nth (* 2 k)          data) 0) ; XXX
          FOR in2 = (or (nth (- N2 1 (* 2 k)) data) 0) ; XXX
      DO
      (setf (aref buf k 1) (+ (* in1 (aref sincos k 0)) (* in2 (aref sincos k 1)))
            (aref buf k 0) (- (* in2 (aref sincos k 0)) (* in1 (aref sincos k 1)))))
    
    ;; complex IFFT, non-scaling
    (process-fft (-> mdct fft) buf nil)
    
    ;; post-IFFT complex multiplication
    (loop FOR k FROM 0 BELOW N4 
          FOR tmp0 = (aref buf k 0)
          FOR tmp1 = (aref buf k 1)
      DO
      (setf (aref buf k 1) (+ (* tmp1 (aref sincos k 0)) (* tmp0 (aref sincos k 1)))
            (aref buf k 0) (- (* tmp0 (aref sincos k 0)) (* tmp1 (aref sincos k 1)))))
    
    ;; reordering
    (loop FOR k FROM 0 BELOW N8 BY 2
          FOR 2k = (* k 2)
      DO
      (setf (aref out (+ out-off 0 2k)) (aref buf (+ N8 0 k) 1)
            (aref out (+ out-off 2 2k)) (aref buf (+ N8 1 k) 1)

            (aref out (+ out-off 1 2k)) (- (aref buf (- N8 1 k) 0))
            (aref out (+ out-off 3 2k)) (- (aref buf (- N8 2 k) 0))

            (aref out (+ out-off N4 0 2k)) (aref buf (+ 0 k) 0)
            (aref out (+ out-off N4 2 2k)) (aref buf (+ 1 k) 0)
            
            (aref out (+ out-off N4 1 2k)) (- (aref buf (- N4 1 k) 1))
            (aref out (+ out-off N4 3 2k)) (- (aref buf (- N4 2 k) 1))
            
            (aref out (+ out-off N2 0 2k)) (aref buf (+ N8 0 k) 0)
            (aref out (+ out-off N2 2 2k)) (aref buf (+ N8 1 k) 0)
            
            (aref out (+ out-off N2 1 2k)) (- (aref buf (- N8 1 k) 1))
            (aref out (+ out-off N2 3 2k)) (- (aref buf (- N8 2 k) 1))
            
            (aref out (+ out-off N2 N4 0 2k)) (- (aref buf (+ 0 k) 1))
            (aref out (+ out-off N2 N4 2 2k)) (- (aref buf (+ 1 k) 1))
            
            (aref out (+ out-off N2 N4 1 2k)) (aref buf (- N4 1 k) 0)
            (aref out (+ out-off N2 N4 3 2k)) (aref buf (- N4 2 k) 0)))
    
    (print buf)
    )
  (print `(:data ,(length data)))
  (values))


