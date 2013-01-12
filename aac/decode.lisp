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

(defun intensity-sign (cb)
  (case cb
    (#.+INTENSITY_HCB+ 1)
    (#.+INTENSITY_HCB2+ -1)
    (otherwise 0)))

(defun invert-intensity (cpe g sfb)
  (if (and (= (-> cpe ms-mask-present) 1)
           #+C (not (eq *audio-object-type* :aac-scalable))
           )
      (- 1 (* 2 (aref (-> cpe ms-used) g sfb)))
    1))

;; 4.6.8.2 Intensity Stereo (IS)
(defun process-is (cpe l-spec r-spec)
  (declare (channel-pair-element cpe))
  (let* ((ics-info (-> cpe ics-info))
         (max-sfb (-> ics-info max-sfb))
         (ics (-> cpe channel-stream2)) ; right XXX: どれを使うのが正しい？
         (sfb-cb (-> ics section-data sfb-cb))
         (dpcm-is-position (-> ics scale-factor-data dpcm-is-position))
         (window-group-length (get-window-group-length ics-info))
         (swb-offset (get-swb-offset ics-info))
         (p 0)
         (new-r-spec '()))
    (loop FOR g FROM 0 BELOW (get-num-window-groups ics-info) 
          FOR is-position = (make-array max-sfb :element-type 'fixnum :initial-element 0) DO
      ;; decode intensity positions for this group
      (loop FOR sfb FROM 0 BELOW max-sfb
            FOR cb = (aref sfb-cb (+ (* g max-sfb) sfb))
            WHEN (is-intensity cb)
            DO (setf (aref is-position sfb) (incf p (aref dpcm-is-position g sfb))))
      
      ;; do intensity stereo decoding
      (loop FOR sfb FROM 0 BELOW max-sfb
            FOR cb = (aref sfb-cb (+ (* g max-sfb) sfb))
            FOR apply? = (is-intensity cb)
            FOR scale = (and apply?
                             (* (intensity-sign cb)
                                (invert-intensity cpe g sfb)
                                (expt 0.5 (* 0.25 (aref is-position sfb)))))
        DO
        (loop WITH width = (- (aref swb-offset (1+ sfb)) (aref swb-offset sfb))
              FOR win FROM 0 BELOW (aref window-group-length g) DO
          (loop FOR bin FROM 0 BELOW width 
                FOR l = (pop l-spec)
                FOR r = (pop r-spec)
            DO
            (if (not apply?)
                (push r new-r-spec)
              (push (* scale l) new-r-spec))))))
    (nreverse new-r-spec)))

;; group->sfb->window-group->window から group->window-group->sfb->window の順に並び替える
(defun sort-data (ics data)
  (let* ((ics-info (-> ics ics-info))
         (window-group-length (get-window-group-length ics-info))
         (swb-offset (get-swb-offset ics-info))
         (new-data '()))
    (loop FOR g FROM 0 BELOW (get-num-window-groups ics-info) DO
      (loop FOR sfb FROM 0 BELOW (-> ics-info max-sfb) DO
        (loop WITH width = (- (aref swb-offset (1+ sfb)) (aref swb-offset sfb))
              FOR win FROM 0 BELOW (aref window-group-length g) DO
          (loop FOR bin FROM 0 BELOW width 
                FOR key = (+ (* g 10000000) (* win 100000) (* sfb 1000) bin) ; XXX:
            DO
            (push `(,key ,(pop data) ,win) new-data)))))

    ;; XXX: 暫定処置: 値生成時に適切な場所に埋めるようにする
    (loop WITH prev-win = 0
          WITH count = 0
          WITH acc = '()
          FOR (key val win) IN (sort new-data #'< :key #'first)
      DO
      (if (= prev-win win)
          (incf count)
        (progn (loop REPEAT (- 128 count) DO (push 0 acc))
               (setf prev-win win)
               (setf count 1)))
      (push val acc)
      FINALLY
      (loop REPEAT (- 128 count) DO (push 0 acc))
      (return (nreverse acc)))))
    ;;(mapcar #'second (sort  new-data #'< :key #'first))))

(defun decode-cpe (cpe &optional (filterbanks (list (make-filterbank) (make-filterbank))))
  (declare (channel-pair-element cpe))
  (let* ((ics1 (-> cpe channel-stream1))
         (ics2 (-> cpe channel-stream2))
         (invquant-data1 (apply-scalefactor ics1 (inverse-quantize-ics ics1)))
         (invquant-data2 (apply-scalefactor ics2 (inverse-quantize-ics ics2))))

    ;; M/S
    (when (and (= 1 (-> cpe common-window))
               (> (-> cpe ms-mask-present) 0))
      (setf (values invquant-data1 invquant-data2)
            (process-m/s cpe invquant-data1 invquant-data2)))

    ;; main prediction
    (assert (eq *audio-object-type* :lc) () "Unsupported audio-object-type: ~a" *audio-object-type*)

    ;; IS
    (setf invquant-data2 (process-is cpe invquant-data1 invquant-data2))

    ;; LTP
    ;; NOTE: AAC-LC don't support LTP

    ;; dependent coupling (before TNS)
    ;; NOTE: 現状、cce未対応なので関係なし

    ;; TNS
    (assert (= 0 (-> ics1 tns-data-present)) () "Unsupported TNS(1)")
    (assert (= 0 (-> ics2 tns-data-present)) () "Unsupported TNS(2)")    

    ;; dependent coupling (after TNS)
    ;; NOTE: 現状、cce未対応なので関係なし
    
    ;; XXX: 
    (setf invquant-data1 (sort-data ics1 invquant-data1))
    (setf invquant-data2 (sort-data ics2 invquant-data2))

    ;; filterbank
    (let ((out1 (process-filterbank (first filterbanks) 
                                    (window-sequence-name (-> ics1 ics-info window-sequence)) 
                                    (-> ics1 ics-info window-shape)
                                    invquant-data1))
          (out2 (process-filterbank (second filterbanks) 
                                    (window-sequence-name (-> ics2 ics-info window-sequence)) 
                                    (-> ics2 ics-info window-shape)
                                    invquant-data2)))
      ;; TODO: ltp

      ;; independent coupling
      ;; NOTE: 現状、cce未対応なので関係なし
      
      ;; gain control
      (assert (= (-> ics1 gain-control-data-present) 0) () "unsupported: gain-control")
      (assert (= (-> ics2 gain-control-data-present) 0) () "unsupported: gain-control")
      
      ;; SBR
      ;; XXX: unsupported
      
      (values out1 out2))))
