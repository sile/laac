(defun test (aac-path &optional (limit 1))
  (with-open-file (in aac-path :element-type '(unsigned-byte 8))
    (let ((in (laac.bit-stream:make in)))
      (laac.adts:each-frame (adts payload) in
        (let ((pin (laac.bit-stream:make-from-bytes payload)))
          (print 
            (list (laac.aac:parse-raw-data-block 
				   pin 
				   :profile (laac.adts:profile-name adts)
				   :sampling-frequency-index (laac.adts::sampling-frequency-index adts))
				  pin
				  (laac.bit-stream:eos? pin)))
		  (unless (plusp (decf limit))
			(return))
          )))))

(defun test2 (aac-path)
  (with-open-file (in aac-path :element-type '(unsigned-byte 8))
    (let ((in (laac.bit-stream:make in))
          (count 0))
      (laac.adts:each-frame (adts payload) in
        (let ((pin (laac.bit-stream:make-from-bytes payload)))
          (laac.aac:parse-raw-data-block 
           pin 
           :profile (laac.adts:profile-name adts)
           :sampling-frequency-index (laac.adts::sampling-frequency-index adts)))
        (incf count))
      count)))

(defun test3 (fn aac-path &optional (limit 1))
  (with-open-file (in aac-path :element-type '(unsigned-byte 8))
    (let ((in (laac.bit-stream:make in)))
      (laac.adts:each-frame (adts payload) in
        (let ((pin (laac.bit-stream:make-from-bytes payload)))
          (funcall fn (laac.aac:parse-raw-data-block 
                       pin 
                       :profile (laac.adts:profile-name adts)
                       :sampling-frequency-index (laac.adts::sampling-frequency-index adts)))
          (unless (plusp (decf limit))
            (return (values)))
          )))))

(defun test4 (aac-path &optional (limit 1))
  (let ((fb1 (laac.aac::make-filterbank))
        (fb2 (laac.aac::make-filterbank)))
  (test3 (lambda (list)
           (let ((cpe (car list)))
             (print (laac.aac::decode-cpe cpe (list fb1 fb2)))
             ))
         aac-path
         limit)))
