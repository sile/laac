(in-package :laac.adts)

(defconstant +HEADER_SIZE+ 7)

(defstruct fixed-header
  (syncword             #xFFF :type (unsigned-byte 12))
  (id                       0 :type (unsigned-byte 1))
  (layer                    0 :type (unsigned-byte 2))
  (protection-absent        0 :type (unsigned-byte 1))
  (profile-object-type      0 :type (unsigned-byte 2))
  (sampling-frequency-index 0 :type (unsigned-byte 4))
  (private-bit              0 :type (unsigned-byte 1))
  (channel-configuration    0 :type (unsigned-byte 3))
  (original-copy            0 :type (unsigned-byte 1))
  (home                     0 :type (unsigned-byte 1)))

(defstruct variable-header
  (copyright-identification-bit       0 :type (unsigned-byte 1))
  (copyright-identification-start     0 :type (unsigned-byte 1))
  (aac-frame-length                   0 :type (unsigned-byte 13))
  (adts-buffer-fullness               0 :type (unsigned-byte 11))
  (number-of-raw-data-blocks-in-frame 0 :type (unsigned-byte 2)))

(defstruct adts
  (fixed-header    t :type fixed-header)
  (variable-header t :type variable-header)
  (raw-data-block  t :type (simple-array (unsigned-byte 8))))

(defun sampling-frequency-index (adts)
  (-> adts fixed-header sampling-frequency-index))

(defun id-name (adts)
  (case (-> adts fixed-header id)
    (0 :mpeg-4)
    (1 :mpeg-2)))

(defun profile-name (adts)
  (case (-> adts fixed-header profile-object-type)
    (0 :main)
    (1 :lc)
    (2 :ssr)
    (3 (if (eq (id-name adts) :mpeg-2)
           :undefined
         :ltp))))
