(in-package :laac.aac)

(defstruct fft
  (length 0 :type fixnum)
  (roots t :type (simple-array t (* *)))
  (rev   t :type (simple-array t (* *)))
  )

(defun new-fft (length)
  (make-fft :length length
            :rev (make-array `(,length 2) :initial-element 0)
            :roots (ecase length
                     (64  *fft-table-64*)
                     (512 *fft-table-512*)
                     (60  *fft-table-60*)
                     (480 *fft-table-480*))))

(defun process-fft (fft in forward?)
  (declare (fft fft))
  (with-slots (length roots rev) fft
    (let* ((im-off (if forward? 2 1))
           (scale  (if forward? length 1)))
    
      ;; bit-reverse
      (loop WITH ii = 0
            FOR i FROM 0 BELOW length
        DO
        (setf (aref rev i 0) (aref in ii 0)
              (aref rev i 1) (aref in ii 1))
        (loop WITH k = (ash length -1)
              WHILE (and (>= ii k)
                         (> k 0))
              DO
              (decf ii k)
              (setf k (ash k -1))
              FINALLY
              (incf ii k)))
      (loop FOR i BELOW length
        DO
        (setf (aref in i 0) (aref rev i 0)
              (aref in i 1) (aref rev i 1)))

      #+C
      (print `(:bit-reversal
               ,(loop FOR i FROM 0 BELOW length
                      COLLECT (list (aref in i 0) (aref in i 1)))))

      ;; bottom base-4 round
      (loop FOR i BELOW length BY 4
        DO
        (let* ((a0 (+ (aref in (+ i 0) 0) (aref in (+ i 1) 0)))
               (a1 (+ (aref in (+ i 0) 1) (aref in (+ i 1) 1)))
               (b0 (+ (aref in (+ i 2) 0) (aref in (+ i 3) 0)))
               (b1 (+ (aref in (+ i 2) 1) (aref in (+ i 3) 1)))
               (c0 (- (aref in (+ i 0) 0) (aref in (+ i 1) 0)))
               (c1 (- (aref in (+ i 0) 1) (aref in (+ i 1) 1)))
               (d0 (- (aref in (+ i 2) 0) (aref in (+ i 3) 0)))
               (d1 (- (aref in (+ i 2) 1) (aref in (+ i 3) 1)))
               (e1-0 (- c0 d1))
               (e1-1 (+ c1 d0))
               (e2-0 (+ c0 d1))
               (e2-1 (- c1 d0)))
          (setf (aref in (+ i 0) 0) (+ a0 b0)
                (aref in (+ i 0) 1) (+ a1 b1)
                (aref in (+ i 2) 0) (- a0 b0)
                (aref in (+ i 2) 1) (- a1 b1))
          
          (if forward?
              (setf (aref in (+ i 1) 0) e2-0
                    (aref in (+ i 1) 1) e2-1
                    (aref in (+ i 3) 0) e1-0
                    (aref in (+ i 3) 1) e1-1)
            (setf (aref in (+ i 1) 0) e1-0
                  (aref in (+ i 1) 1) e1-1
                  (aref in (+ i 3) 0) e2-0
                  (aref in (+ i 3) 1) e2-1))))

      #+C
      (print `(:bottom-base-4-round
               ,(loop FOR i FROM 0 BELOW length
                      COLLECT (list (aref in i 0) (aref in i 1)))))

      ;; iterations from bottom to top
      (loop FOR i = 4 THEN (ash i 1)
            WHILE (< i length)
            FOR shift = (ash i 1)
            FOR m = (floor length shift)
        DO
        (loop FOR j FROM 0 BELOW length BY shift
          DO
          (loop FOR k FROM 0 BELOW i
                FOR km = (* k m)
                FOR root-re = (aref roots km 0)
                FOR root-im = (aref roots km im-off)
                FOR z-re = (- (* (aref in (+ i j k) 0) root-re) (* (aref in (+ i j k) 1) root-im))
                FOR z-im = (+ (* (aref in (+ i j k) 0) root-im) (* (aref in (+ i j k) 1) root-re))
            DO
            (setf (aref in (+ i j k) 0) (* (- (aref in (+ j k) 0) z-re) scale)
                  (aref in (+ i j k) 1) (* (- (aref in (+ j k) 1) z-im) scale)
                  (aref in (+ j k) 0)   (* (+ (aref in (+ j k) 0) z-re) scale)
                  (aref in (+ j k) 1)   (* (+ (aref in (+ j k) 1) z-im) scale)))))

      #+C
      (print `(:iterations-from-bottom-to-top
               ,(loop FOR i FROM 0 BELOW length
                      COLLECT (list (aref in i 0) (aref in i 1)))))
      (values))))
