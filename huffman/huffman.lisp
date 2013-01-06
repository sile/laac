(in-package :laac.huffman)

(eval-when (:compile-toplevel :load-toplevel :execute)
           (rename-package :laac.bit-stream :laac.bit-stream '(:bit-stream)))

(defun find-offset (in table)
  (declare (bit-stream:bit-stream in)
           ((array t #|fixnum|# (* *)) table))
  (loop WITH len = 0
        WITH cw = 0
        FOR off fixnum FROM 0
        DO
	(let ((j (- (aref table off +LENGTH_POS+) len)))
	  (incf len j)
	  (setf cw (+ (ash cw j) 
                      (bit-stream:read-bits in j)))
	  (when (= cw (aref table off +CODEWORD_POS+))
            (return off)))))

(defun decode-scale-factor (in)
  (declare (bit-stream:bit-stream in))
  (aref *hcb-sf* (find-offset in *hcb-sf*) +INDEX_POS+))

(defun decode-spectral-data (in codebook-number)
  (declare (bit-stream:bit-stream in))
  (multiple-value-bind (unsigned? dimention lav codebook)
                       (get-codebook codebook-number)
    (let* ((offset (find-offset in codebook))
           (index (aref codebook offset +INDEX_POS+)))
      (multiple-value-bind (w x y z) (get-wxyz unsigned? dimention lav index)
        (when unsigned?
          (when (= dimention 4)
            (and (/= 0 w) (bit-stream:read-bool in) (setf w (- w)))
            (and (/= 0 x) (bit-stream:read-bool in) (setf x (- x))))
          (and (/= 0 y) (bit-stream:read-bool in) (setf y (- y)))
          (and (/= 0 z) (bit-stream:read-bool in) (setf z (- z))))
        (values w x y z)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package :laac.bit-stream :laac.bit-stream '()))
