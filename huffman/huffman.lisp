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
		
(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package :laac.bit-stream :laac.bit-stream '()))
