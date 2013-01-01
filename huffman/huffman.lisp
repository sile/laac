(in-package :laac.huffman)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package :laac.bit-stream :laac.bit-stream '(:bit-stream)))

(defun find-offset (in table)
  (declare (bit-stream:bit-stream in)
		   ((array fixnum (* *)) table))
  (loop WITH len = (aref table 0 +LENGTH_POS+)
		WITH cw = (bit-stream:read-bits in len)
		FOR off fixnum FROM 1
		WHILE (/= cw (aref table off +CODEWORD_POS+))
    DO
	(let ((j (- (aref table off +LENGTH_POS+) len)))
	  (setf cw (+ (ash cw j) 
				  (bit-stream:read-bits in j))))
	FINALLY
	(return off)))

(defun decode-scale-factor (in)
  (declare (bit-stream:bit-stream in))
  (aref *hcb-sf* (find-offset in *hcb-sf*) +INDEX_POS+))
		
(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package :laac.bit-stream :laac.bit-stream '()))
