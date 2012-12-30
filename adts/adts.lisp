(in-package :laac.adts)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package :laac.bit-stream :laac.bit-stream '(:bit-stream)))

(defun parse-fixed-header (in)
  (declare (bit-stream:bit-stream in))
  (with-oop-like (in :bit-stream)
    (let ((syncword (in read-bits 12)))
      (assert (= #xFFF syncword) () "syncword must be #xFFF: value=~d" syncword)
    
      (make-fixed-header
       :syncword syncword
       :id                       (in read-bits 1)
       :layer                    (in read-bits 2)
       :protection-absent        (in read-bits 1) ; TODO: support protection-absent==0
       :profile-object-type      (in read-bits 2)
       :sampling-frequency-index (in read-bits 4)
       :private-bit              (in read-bits 1)
       :channel-configuration    (in read-bits 3)
       :original-copy            (in read-bits 1)
       :home                     (in read-bits 1)
       ))))

(defun parse-variable-header (in)
  (declare (bit-stream:bit-stream in))
  (with-oop-like (in :bit-stream)
    (make-variable-header
     :copyright-identification-bit       (in read-bits 1)
     :copyright-identification-start     (in read-bits 1)
     :aac-frame-length                   (in read-bits 13)
     :adts-buffer-fullness               (in read-bits 11)
     :number-of-raw-data-blocks-in-frame (in read-bits 2))))

(defun parse (stream)
  (declare (bit-stream:bit-stream stream))
  (let* ((fixed-header (parse-fixed-header stream))
         (variable-header (parse-variable-header stream))
         (raw-data-block (bit-stream:read-bytes stream (- (-> variable-header aac-frame-length)
                                                          +HEADER_SIZE+))))
    (make-adts :fixed-header fixed-header
               :variable-header variable-header
               :raw-data-block raw-data-block)))

(defmacro each-frame ((adts raw-data-block) stream &body body)
  `(loop UNTIL (bit-stream:eos? ,stream)
         FOR ,adts = (parse ,stream)
         FOR ,raw-data-block = (-> ,adts raw-data-block)
         DO (locally ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package :laac.bit-stream :laac.bit-stream '()))
