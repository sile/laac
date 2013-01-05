(in-package :laac.aac)

(defun sign (n)
  (cond ((> n 0)  1)
        ((< n 0) -1)
        (t        0)))

;; 4.6.1.3 Decoding process
(defun inverse-quantize (n)
  (* (sign n) (expt (abs n) 4/3)))

(defun inverse-quantize-ics (ics)
  (declare (channel-stream ics))
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
              WITH cb = (aref sfb-cb (+ (* g (-> ics-info max-sfb)) sfb))
              FOR win FROM 0 BELOW (aref window-group-length g) DO
          (loop FOR bin FROM 0 BELOW width DO
            (assert (/= cb +NOISE_HCB+) () "Unsupport NOISE_HCB")
            (if (/= cb +ZERO_HCB+ +INTENSITY_HCB+ +INTENSITY_HCB2+)
                (push (inverse-quantize (pop data)) acc)
              (push 0.0 acc))
            ))))
    (assert (null data) () "too many quantized data")
    (nreverse acc)))

;; 4.6.2.3.2 Decoding of scalefactors
(defun decode-scalefactors (ics)
  (declare (channel-stream ics))
  (let* ((last-sf (-> ics global-gain))
         (ics-info (-> ics ics-info))
         (section-data (-> ics section-data))
         (scale-factor-data (-> ics scale-factor-data))
         (dpcm-sf-base (-> scale-factor-data dpcm-sf))
         (max-sfb (-> ics-info max-sfb))
         (num-window-groups (get-num-window-groups ics-info))
         (sf (make-array `(,num-window-groups ,max-sfb) :element-type 'fixnum)))
    (loop FOR g FROM 0 BELOW num-window-groups DO
      (loop FOR sfb FROM 0 BELOW max-sfb
            FOR sfb-cb = (aref (-> section-data sfb-cb)
                               (+ (* g max-sfb) sfb)) DO
        (if (/= sfb-cb +ZERO_HCB+ +INTENSITY_HCB+ +INTENSITY_HCB2+)
            (let* ((index-offset -60) ; Table 4.131 – Scalefactor Huffman codebook parameters
;;                  (max-value 60)
;;                  (min-value -60)
                   (dpcm-sf (+ (aref dpcm-sf-base g sfb) index-offset))
                   )
              (setf (aref sf g sfb) (+ dpcm-sf last-sf)
                    last-sf (aref sf g sfb)))
          (setf (aref sf g sfb) 0))))
    sf))

(defconstant +SF_OFFSET+ 100)
(defun get-scale-factor-gain (sf)
  (expt 2 (* 0.25 (- sf +SF_OFFSET+))))

;; 4.6.2.3.3 Applying scalefactors
(defun apply-scalefactor (ics invquant-data)
  (declare (channel-stream ics))
  (let* ((ics-info (-> ics ics-info))
         (window-group-length (get-window-group-length ics-info))
         (swb-offset (get-swb-offset ics-info))
         (sf (decode-scalefactors ics))
         (rescaled-data '()))
    (loop FOR g FROM 0 BELOW (get-num-window-groups ics-info) DO
      (loop FOR sfb FROM 0 BELOW (-> ics-info max-sfb) DO
        (loop WITH width = (- (aref swb-offset (1+ sfb)) (aref swb-offset sfb))
              FOR win FROM 0 BELOW (aref window-group-length g) DO
          (loop WITH gain = (get-scale-factor-gain (aref sf g sfb))
                FOR bin FROM 0 BELOW width DO
            (push (* gain (pop invquant-data)) rescaled-data)))))
    (nreverse rescaled-data)))

(defun is-intensity (cb)
  (or (= cb +INTENSITY_HCB+)
      (= cb +INTENSITY_HCB2+)))

(defun is-noise (cb)
  (= cb +NOISE_HCB+))

;; 4.6.8.1 M/S stereo
(defun process-m/s (cpe l-spec r-spec)
  (declare (channel-pair-element cpe))
  (let* ((ics-info (-> cpe ics-info))
         (sfb-cb1 (-> cpe channel-stream1 section-data sfb-cb))
         (sfb-cb2 (-> cpe channel-stream2 section-data sfb-cb))
         (window-group-length (get-window-group-length ics-info))
         (swb-offset (get-swb-offset ics-info))
         (ms-used (-> cpe ms-used))
         (new-l-spec '())
         (new-r-spec '()))
    (loop FOR g FROM 0 BELOW (get-num-window-groups ics-info) DO
      (loop WITH max-sfb = (-> ics-info max-sfb) 
            FOR sfb FROM 0 BELOW max-sfb
            FOR cb1 = (aref sfb-cb1 (+ (* g max-sfb) sfb))
            FOR cb2 = (aref sfb-cb2 (+ (* g max-sfb) sfb))
            FOR apply? = (and (or (= (aref ms-used g sfb) 1)
                                  (= (-> cpe ms-mask-present) 2))
                              (not (is-intensity cb1))
                              (not (is-intensity cb2))
                              (not (is-noise cb1))
                              (not (is-noise cb2)))
        DO
        (loop WITH width = (- (aref swb-offset (1+ sfb)) (aref swb-offset sfb))
              FOR win FROM 0 BELOW (aref window-group-length g) DO
          (loop FOR bin FROM 0 BELOW width DO
            (if (not apply?)
                (progn (push (pop l-spec) new-l-spec)
                       (push (pop r-spec) new-r-spec))
              (let* ((l (pop l-spec))
                     (r (pop r-spec)))
                (push (+ l r) new-l-spec)
                (push (- l r) new-r-spec)))))))
    (values (nreverse new-l-spec)
            (nreverse new-r-spec))))

(defun decode-cpe (cpe)
  (declare (channel-pair-element cpe))
  (let* ((ics1 (-> cpe channel-stream1))
         (ics2 (-> cpe channel-stream2))
         (invquant-data1 (apply-scalefactor ics1 (inverse-quantize-ics ics1)))
         (invquant-data2 (apply-scalefactor ics2 (inverse-quantize-ics ics2))))
    
    (when (and (= 1 (-> cpe common-window))
               (> (-> cpe ms-mask-present) 0))
      (setf (values invquant-data1 invquant-data2)
            (process-m/s cpe invquant-data1 invquant-data2)))
    
    (values invquant-data1
            invquant-data2)))