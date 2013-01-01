(defun test (aac-path)
  (with-open-file (in aac-path :element-type '(unsigned-byte 8))
    (let ((in (laac.bit-stream:make in)))
      (laac.adts:each-frame (adts payload) in
        (let ((pin (laac.bit-stream:make-from-bytes payload)))
          (print 
            (list (laac.aac:parse-raw-data-block pin :profile (laac.adts:profile-name adts))
              pin))
          (return)
          )))))
