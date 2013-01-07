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
    
    
