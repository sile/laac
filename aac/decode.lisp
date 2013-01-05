(in-package :laac.aac)

(defun sign (n)
  (cond ((> n 0)  1)
        ((< n 0) -1)
        (t        0)))

;; 4.6.1.3 Decoding process
(defun inverse-quantize (n)
  (* (sign n) (expt (abs n) 4/3)))

(defun inverse-quantize-ics (ics)
  (declare (channel-stream ics)
           (optimize (debug 3))) ; XXX:
  (let* ((ics-info (-> ics ics-info))
         (section-data (-> ics section-data))
         (spectral-data (-> ics spectral-data))
         (data (copy-seq (-> spectral-data data)))
         (window-group-length (get-window-group-length ics-info))
         (swb-offset (get-swb-offset ics-info))
         (sfb-cb (-> section-data sfb-cb))
         (acc '()))
    (loop FOR g FROM 0 BELOW (get-num-window-groups ics-info) DO
      (loop FOR sfb FROM 0 BELOW (-> ics-info max-sfb) DO
        (loop WITH width = (- (aref swb-offset (1+ sfb)) (aref swb-offset sfb))
              WITH cb = (aref sfb-cb sfb)
              FOR win FROM 0 BELOW (aref window-group-length g) DO
          (loop FOR bin FROM 0 BELOW width DO
            (assert (/= cb +NOISE_HCB+) () "Unsupport NOISE_HCB")
            (if (/= cb +ZERO_HCB+ +INTENSITY_HCB+ +INTENSITY_HCB2+)
                (push (inverse-quantize (pop data)) acc)
              (push 0.0 acc))
            ))))
    (assert (null data) () "too many quantized data")
    (nreverse acc)))
