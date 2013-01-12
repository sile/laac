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
             (laac.aac::decode-cpe cpe (list fb1 fb2))
             ))
         aac-path
         limit)))

(defun @write-string (out str)
  (write-sequence (string-to-octets str) out))

(defun @write-uint4 (out n)
  (write-byte (ldb (byte 8 0) n) out)
  (write-byte (ldb (byte 8 8) n) out)
  (write-byte (ldb (byte 8 16) n) out)
  (write-byte (ldb (byte 8 24) n) out))

(defun @write-uint2 (out n)
  (write-byte (ldb (byte 8 0) n) out)
  (write-byte (ldb (byte 8 8) n) out))

(defun write-wav (out pcm)
  (let* ((pcm-byte-size (* (length pcm) 2))
         (file-size (+ 4 ; wave
                       4 ; fmt 
                       4 ; bytes of fmt
                       2 ; format id
                       2 ; channels
                       4 ; sampling rate
                       4 ; byte rate
                       2 ; block size
                       2 ; bits per sample
                       
                       4 ; data
                       4 ; data-size
                       pcm-byte-size))
         )
      (@write-string out "RIFF")
      (@write-uint4 out file-size)
      (@write-string out "WAVE")
      (@write-string out "fmt ")
      (@write-uint4 out 16)
      (@write-uint2 out 1)
      (@write-uint2 out 2)
      (@write-uint4 out 22050)
      (@write-uint4 out (* 22050 2 2))
      (@write-uint2 out (* 2 2))
      (@write-uint2 out 16)
      (@write-string out "data")
      (@write-uint4 out pcm-byte-size)
      (dolist (d pcm)
        (@write-uint2 out d))))

(defun decode-to-wav (aac-path wav-path)
  (let ((fb1 (laac.aac::make-filterbank))
        (fb2 (laac.aac::make-filterbank))
        (pcm '()))
    (test3 (lambda (list)
             (let ((cpe (car list)))
               (multiple-value-bind (l-data r-data) (laac.aac::decode-cpe cpe (list fb1 fb2))
                 (dotimes (i (length l-data))
                   (let ((l (min (max (round (aref l-data i)) #x-7FFF) #x7FFF))
                         (r (min (max (round (aref r-data i)) #x-7FFF) #x7FFF)))
                     (push r pcm)
                     (push l pcm))))))
           aac-path
           most-positive-fixnum)
    
    (with-open-file (out wav-path
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))
      (write-wav out (nreverse pcm))
      ))
  t)
