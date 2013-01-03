(in-package :laac.aac)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package :laac.huffman :laac.huffman '(:huffman))
  (rename-package :laac.bit-stream :laac.bit-stream '(:bit-stream)))

;; XXX:
(defparameter *audio-object-type* :undefined) ;; (member :lc :main :ssr :ltp :undefined)

;; XXX: adtsの場合は、この値はどこで設定される？
(defparameter *aac-section-data-resillience-flag* 0)
(defparameter *aac-scale-factor-data-resilience-flag* 0)
(defparameter *aac-spectral-data-resilience-flag* 0)

;; XXX: この値はどこで定義されている？
(defconstant +PRED_SFB_MAX+ most-positive-fixnum)

(defun parse-ics-info (in common-window)
  (declare (bit-stream:bit-stream in)
           (ignore common-window))
  (with-oop-like (in :bit-stream)
    (let ((ics-reserved-bit (in read-bits 1))
          (window-sequence (in read-bits 2))
          (window-shape (in read-bits 1)))
      (if (eq (window-sequence-name window-sequence) :eight-short-sequence)
          (make-ics-info :ics-reserved-bit ics-reserved-bit
                         :window-sequence window-sequence
                         :window-shape window-shape
                         :max-sfb (in read-bits 4)
                         :scale-factor-grouping (in read-bits 7))
        (let ((max-sfb (in read-bits 6))
              (predictor-data-present (in read-bits 1))
              (predictor-reset 0)
              (predictor-reset-group-number 0)
              (prediction-used nil))
          (when (= 1 predictor-data-present)
            (assert (eq :lc *audio-object-type*) () "Unsupported AudioObjectType: ~a" *audio-object-type*)
            (setf predictor-reset (in read-bits 1))
            (when (= 1 predictor-reset)
              (setf predictor-reset-group-number (in read-bits 5)
                    prediction-used (make-array (min max-sfb +PRED_SFB_MAX+)))
              (dotimes (i (length prediction-used))
                (setf (aref prediction-used i) (in read-bits 1))))
            )

          (make-ics-info :ics-reserved-bit ics-reserved-bit
                         :window-sequence window-sequence
                         :window-shape window-shape
                         :max-sfb max-sfb
                         :predictor-data-present predictor-data-present
                         :predictor-reset predictor-reset
                         :predictor-reset-group-number predictor-reset-group-number
                         :prediction-used prediction-used)
          )))))

(defun parse-section-data (in ics-info)
  (declare (bit-stream:bit-stream in)
           (ics-info ics-info))
  (with-oop-like (in :bit-stream)
    (let* ((eight-short-sequence? (eq (window-sequence-name (-> ics-info window-sequence))
                                      :eight-short-sequence))
           (sect-esc-val (if eight-short-sequence?
                             (1- (ash 1 3))
                           (1- (ash 1 5))))
           (num-window-groups (get-num-window-groups ics-info))
           (sect-cb-list '())
           (sect-start-list '())
           (sect-end-list '())
           (sfb-cb-list '())
           (num-sec-list '()))
      
      (dotimes (g num-window-groups)
        (loop WITH k = 0
              FOR i fixnum FROM 0
              WHILE (< k (-> ics-info max-sfb))
          DO
          (let ((sect-cb (if (= *aac-section-data-resillience-flag* 1)
                             (in read-bits 5)
                           (in read-bits 4)))
                (sect-len 0))
            
            (if (or (= 0 *aac-section-data-resillience-flag*)
                    (< sect-cb 11)
                    (< 11 sect-cb 16))
                (loop FOR sect-len-incr = (if eight-short-sequence? 
                                              (in read-bits 3)
                                            (in read-bits 5))
                      DO (incf sect-len sect-len-incr)
                      WHILE (= sect-len-incr sect-esc-val))
              (incf sect-len))

            (push sect-cb sect-cb-list)
            (push k sect-start-list)
            (push (+ k sect-len) sect-end-list)
            
            (loop FOR sfb FROM k BELOW (+ k sect-len)
                  DO (push sect-cb sfb-cb-list))
            
            (incf k sect-len)
            )
          FINALLY
          (push i num-sec-list)))
          
      (make-section-data :sect-cb (coerce (nreverse sect-cb-list) '(vector (unsigned-byte 5)))
                         :sfb-cb  (coerce (nreverse sfb-cb-list) '(vector (unsigned-byte 5)))
                         :sect-start (coerce (nreverse sect-start-list) '(vector fixnum))
                         :sect-end   (coerce (nreverse sect-end-list) '(vector fixnum))
                         :num-sec    (coerce (nreverse num-sec-list) '(vector fixnum))
                         )
      )))

(defun parse-scale-factor-data (in section-data ics-info)
  (declare (bit-stream:bit-stream in)
                   (section-data section-data)
           (ics-info ics-info))
  (with-oop-like (in :bit-stream)
    (let ((sfb-cb (-> section-data sfb-cb))
                  (dpcm-is-position #1=(make-array `(,(get-num-window-groups ics-info) ,(-> ics-info max-sfb)) 
                                                                          :initial-element 0 :element-type 'fixnum))
                  (dpcm-noise-nrg #1#)
                  (dpcm-sf #1#))
          
          (assert (= 0 *aac-scale-factor-data-resilience-flag*) ()
                  "Unsupported: aacScaleFactorDataResilienceFlag == 1")

          (loop WITH noise-pcm-flag = 1
                        WITH num-window-groups = (get-num-window-groups ics-info)
                        FOR g FROM 0 BELOW num-window-groups
            DO
                (loop FOR sfb-index FROM 0 BELOW (-> ics-info max-sfb)
                          FOR sfb = (aref sfb-cb (+ (* g num-window-groups) sfb-index))
                  DO
                  (case sfb
                    ((#.+ZERO_HCB+))
                        ((#.+INTENSITY_HCB+ #.+INTENSITY_HCB2+) ; is_intensity(g, sfb-index) == true
                         (setf (aref dpcm-is-position g sfb-index) (huffman:decode-scale-factor in)))
                        ((#.+NOISE_HCB+) ; is_notice(g, sfb-index) == true
                         (if (= noise-pcm-flag 1)
                                 (setf (aref dpcm-noise-nrg g sfb-index) (in read-bits 9)
                                           noise-pcm-flag 0)
                           (setf (aref dpcm-noise-nrg g sfb-index) (huffman:decode-scale-factor in))))
                        (otherwise
                         (setf (aref dpcm-sf g sfb-index) (huffman:decode-scale-factor in)))
                        )))
      (make-scale-factor-data :dpcm-is-position dpcm-is-position
                              :dpcm-noise-nrg dpcm-noise-nrg
                              :dpcm-sf dpcm-sf))))

(defun parse-gain-control-data (in ics-info)
  (declare (bit-stream:bit-stream in)
           (ics-info ics-info))
  (with-oop-like (in :bit-stream)
    (let ((max-band (in read-bits 2))
          (adjust-num-list '())
          (alevcode-list '())
          (aloccode-list '()))
      
      (labels ((do-common (wd-count body-fn)
                 (loop REPEAT wd-count DO 
                   (push 0 adjust-num-list)
                   (push 0 alevcode-list)
                   (push 0 aloccode-list))
               
                 (loop FOR bd FROM 1 TO max-band DO
                   (loop FOR wd FROM 0 BELOW wd-count DO
                     (push (in read-bits 3) adjust-num-list)
                     (loop FOR ad BELOW (car adjust-num-list) DO
                       (funcall body-fn bd wd ad)))))

               (do-only-long-sequence ()
                 (do-common 1 (lambda (bd wd ad)
                                (declare (ignore bd wd ad))
                                (push (in read-bits 4) alevcode-list)
                                (push (in read-bits 5) aloccode-list))))
               (do-long-start-sequence ()
                 (do-common 2 (lambda (bd wd ad)
                                (declare (ignore bd ad))
                                (push (in read-bits 4) alevcode-list)
                                (if (= wd 0)
                                    (push (in read-bits 4) aloccode-list)
                                  (push (in read-bits 2) aloccode-list)))))
               (do-eight-short-sequence ()
                 (do-common 8 (lambda (bd wd ad)
                                (declare (ignore bd wd ad))
                                (push (in read-bits 4) alevcode-list)
                                (push (in read-bits 2) aloccode-list))))
               (do-long-stop-sequence ()
                 (do-common 2 (lambda (bd wd ad)
                                (declare (ignore bd ad))
                                (push (in read-bits 4) alevcode-list)
                                (if (= wd 0)
                                    (push (in read-bits 4) aloccode-list)
                                  (push (in read-bits 5) aloccode-list))))))
        (case (window-sequence-name (-> ics-info window-sequence))
          (:only-long-sequence (do-only-long-sequence))
          (:long-start-sequence (do-long-start-sequence))
          (:eight-short-sequence (do-eight-short-sequence))
          (:long-stop-sequence (do-long-stop-sequence))))
  
      (make-gain-control-data :max-band max-band
                              :adjust-num (coerce (nreverse adjust-num-list) '(vector (unsigned-byte 3)))
                              :alevcode   (coerce (nreverse alevcode-list) '(vector (unsigned-byte 4)))
                              :aloccode   (coerce (nreverse aloccode-list) '(vector (unsigned-byte 5)))))))

(defun get-escape (in minus?)
  (declare (bit-stream:bit-stream in))  
  (let ((i 4))
    (loop WHILE (bit-stream:read-bool in)
          DO (incf i))
    (let ((j (bit-stream:read-bits in i)))
      (when (= j 0)
        (setf j (ash 1 i)))
      (if minus? (- j) j))))

(defun parse-spectral-data (in ics-info section-data)
  (declare (bit-stream:bit-stream in)
           (ics-info ics-info)
           (section-data section-data))
  (let ((sect-sfb-offset (get-sect-sfb-offset ics-info))
        (acc '())
        (total-i -1))
    (dotimes (g (get-num-window-groups ics-info))
      (dotimes (i (aref (-> section-data num-sec) g))
        (incf total-i)
        (let* ((g-i total-i)
               (sect-cb (aref (-> section-data sect-cb) g-i)))
          (when (/= sect-cb +ZERO_HCB+ +NOISE_HCB+ +INTENSITY_HCB+ +INTENSITY_HCB2+)
            (loop WITH start-index = (aref (-> section-data sect-start) g-i)
                  WITH end-index   = (aref (-> section-data sect-end) g-i)
                  FOR k FROM (aref sect-sfb-offset g start-index) BELOW (aref sect-sfb-offset g end-index) 
                      BY (if (< sect-cb +FIRST_PAIR_HCB+) +QUAD_LEN+ +PAIR_LEN+)
              DO
              (multiple-value-bind (w x y z) (huffman:decode-spectral-data in sect-cb)
                (when (= sect-cb +ESC_HCB+)
                  (when (= (abs y) +ESC_FLAG+)
                    (setf y (get-escape in (minusp y))))
                  (when (= (abs z) +ESC_FLAG+)
                    (setf z (get-escape in (minusp z))))
                  )
                (push `(:g ,g :i ,i :w ,w :x ,x :y ,y :z ,z) acc)
                )
              )))))
    (make-spectral-data :data (nreverse acc))))

(defun parse-individual-channel-stream (in common-window scale-flag common-ics-info)
  (declare (bit-stream:bit-stream in)
           ((unsigned-byte 1) common-window scale-flag))
  (with-oop-like (in :bit-stream)
    (let ((global-gain (in read-bits 8))
          (ics-info nil)
          (section-data nil)
          (scale-factor-data nil)
          (pulse-data-present 0)
          (tns-data-present 0)
          (gain-control-data-present 0)
          (gain-control-data nil)
                  (spectral-data nil))
      (when (and (= common-window 0) (= scale-flag 0))
        (setf ics-info (parse-ics-info in common-window)))
      
      (setf section-data (parse-section-data in (or ics-info common-ics-info)))
          (setf scale-factor-data (parse-scale-factor-data in section-data (or ics-info common-ics-info)))
      
      (when (= 0 scale-flag)
        (setf pulse-data-present (in read-bits 1))
        (when (= 1 pulse-data-present)
          (error "TODO: not implemented: pulse-data"))

        (setf tns-data-present (in read-bits 1))
        (when (= 1 tns-data-present)
          (error "TODO: not implemented: tns-data"))

        (setf gain-control-data-present (in read-bits 1))
        (when (= 1 gain-control-data-present)
          (setf gain-control-data (parse-gain-control-data in (or ics-info common-ics-info))))
        )

          (assert (= *aac-spectral-data-resilience-flag* 0) ()
                          "Unsupported: aacSpectralDataResilienceFlag == 1")
          
          (setf spectral-data (parse-spectral-data in (or ics-info common-ics-info) section-data))
          
      (make-channel-stream :global-gain global-gain
                           :ics-info ics-info
                           :section-data section-data
                           :scale-factor-data scale-factor-data
                           :pulse-data-present pulse-data-present
                           :tns-data-present tns-data-present
                           :gain-control-data-present gain-control-data-present
                           :gain-control-data gain-control-data
                           :spectral-data spectral-data
                           )
      )))


(defun parse-single-channel-element (in)
  (declare (bit-stream:bit-stream in)
           (ignore in))
  :sce)

(defun parse-channel-pair-element (in)
  (declare (bit-stream:bit-stream in))
  (with-oop-like (in :bit-stream)
    (let ((element-instance-tag (in read-bits 4))
          (common-window        (in read-bits 1))
          (ics-info nil)
          (ms-mask-present 0)
          (ms-used nil))
      (when (= common-window 1)
        (setf ics-info        (parse-ics-info in common-window)
              ms-mask-present (in read-bits 2))
        (when (= ms-mask-present 1)
          (let ((num-window-groups (get-num-window-groups ics-info))
                (max-sfb (-> ics-info max-sfb)))
            (setf ms-used (make-array `(,num-window-groups ,max-sfb) :element-type '(unsigned-byte 1)))
            (dotimes (g num-window-groups)
              (dotimes (sfb max-sfb)
                (setf (aref ms-used g sfb) (in read-bits 1)))))
          ))
      (make-channel-pair-element
       :element-instance-tag element-instance-tag
       :common-window        common-window
       :ics-info             ics-info
       :ms-mask-present      ms-mask-present
       :ms-used              ms-used
       :channel-stream1 (parse-individual-channel-stream in common-window 0 ics-info)
       :channel-stream2 (parse-individual-channel-stream in common-window 0 ics-info)
       ))))

(defun parse-coupling-channel-element (in)
  (declare (bit-stream:bit-stream in) (ignore in))
  :cce)

(defun parse-program-config-element (in)
  (declare (bit-stream:bit-stream in) (ignore in))
  :pce)

(defun parse-lfe-channel-element (in)
  (declare (bit-stream:bit-stream in) (ignore in))
  :lfe)

(defun parse-fill-element (in)
  (declare (bit-stream:bit-stream in) (ignore in))
  :fil)

(defun parse-raw-data-block (in &key (profile :undefined) (sampling-frequency-index))
  (declare (bit-stream:bit-stream in))
  (prog1
      (loop WITH *audio-object-type* = profile
                        WITH *sampling-frequency-index* = sampling-frequency-index
            FOR id-syn-ele = (syntactic-element-name (bit-stream:read-bits in 3))
            UNTIL (eq id-syn-ele :end)
        COLLECT
        (case id-syn-ele
          ((:dse) (error "TODO: unsupported id-syn-ele: ~a" id-syn-ele))
          (:sce (parse-single-channel-element in))
          (:cpe (parse-channel-pair-element in))
          (:cce (parse-coupling-channel-element in))
          (:lfe (parse-lfe-channel-element in))
          (:pce (parse-program-config-element in))
          (:fil (parse-fill-element in))
          ))
    (bit-stream:drop-unaligned-bits in)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package :laac.huffman :laac.huffman '())
  (rename-package :laac.bit-stream :laac.bit-stream '()))
