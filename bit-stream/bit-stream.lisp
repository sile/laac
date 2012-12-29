(in-package :laac.bit-stream)

(declaim (inline read-bits-impl))

(defstruct bit-stream
  (source    t :type stream)
  (cur-byte  t :type (unsigned-byte 8))
  (rest-bits t :type (integer 0 8)))

(defun make (source-stream)
  (make-bit-stream :source source-stream
                   :cur-byte 0
                   :rest-bits 0))

(defun eos? (stream)
  (declare (bit-stream stream))
  (with-slots (source cur-byte rest-bits) stream
    (and (= rest-bits 0)
         (let ((byte (read-byte source nil :eos)))
           (unless (eq byte :eos)
             (setf cur-byte byte
                   rest-bits 8))
           (eq byte :eos)))))

(defmacro read-bits (stream bit-count &key signed)
  (declare (ignore signed))
  (if (typep bit-count 'fixnum)
      `(the (unsigned-byte ,bit-count) (read-bits-impl ,stream ,bit-count))
    `(read-bits-impl ,stream ,bit-count)))
     
(defun read-next-byte-if-need (stream)
  (declare (bit-stream stream))
  (with-slots (source cur-byte rest-bits) stream
    (when (= rest-bits 0)
      (setf cur-byte (read-byte source)
            rest-bits 8))
    t))

(defun read-bits-impl (stream bit-count)
  (declare (bit-stream stream)
           (fixnum bit-count))
  (labels ((recur (bit-count acc)
             (if (= 0 bit-count)
                 acc
               (with-slots (cur-byte rest-bits) stream
                 (read-next-byte-if-need stream)
                 (let ((read-bits (min rest-bits bit-count)))
                   (decf rest-bits read-bits)
                   (recur (- bit-count read-bits) 
                          (+ (ash acc read-bits)
                             (ldb (byte read-bits rest-bits) cur-byte))))))))
    (recur bit-count 0)))
