(in-package :laac.aac)

;; 1.6.4.3
(defun get-sampling-frequeny (index)
  (declare ((integer 0 16) index))
  (case index
    (#x0 96000)
    (#x1 88200)
    (#x2 64000)
    (#x3 48000)
    (#x4 44100)
    (#x5 32000)
    (#x6 24000)
    (#x7 22050)
    (#x8 16000)
    (#x9 12000)
    (#xa 11025)
    (#xb  8000)
    (#xc  7350)
    ((#xd #xe) (error "reserved sample frequency index: ~a" index))
    ((#xf)     (error "escale value: ~a" index)))) ; XXX: どのようなケースでこの値が使用されるか？

;; 4.5.4 Tables
(defparameter *swb-offset-long-96*
  #(0 4 8 12 16 20 24 28 32 36 40 44 48 52 56 64 72 80 88 96 108 
    120 132 144 156 172 188 212 240 276 320 384 448 512 576 640
    704 768 832 896 960 1024))

(defparameter *swb-offset-short-96*
  #(0 4 8 12 16 20 24 32 40 48 64 92 128))

(defparameter *swb-offset-long-64*
  #(0 4 8 12 16 20 24 28 32 36 40 44 48 52 56 64 72 80 88 100 112
    124 140 156 172 192 216 240 268 304 344 384 424 464 504 544 
    584 624 664 704 744 784 824 864 904 944 984 1024))

(defparameter *swb-offset-short-64*
  #(0 4 8 12 16 20 24 32 40 48 64 92 128))

(defparameter *swb-offset-long-48*
  #(0 4 8 12 16 20 24 28 32 36 40 48 56 64 72 80 88 96 
    108 120 132 144 160 176 196 216 240 264 292 320 352
    384 416 448 480 512 544 576 608 640 672 704 736
    768 800 832 864 896 928 1024))

(defparameter *swb-offset-short-48*
  #(0 4 8 12 16 20 28 36 44 56 68 80 96 112 128))

(defparameter *swb-offset-long-32*
  #(0 4 8 12 16 20 24 28 32 36 40 48 56 64 72 80 88 96
    108 120 132 144 160 176 196 216 240 264 292 320 352
    384 416 448 480 512 544 576 608 640 672 704 736
    768 800 832 864 896 928 960 992 1024))

(defparameter *swb-offset-short-32* 
  *swb-offset-short-48*)

(defparameter *swb-offset-long-24*
  #(0 4 8 12 16 20 24 28 32 36 40 44 52 60 68 76 84 92 100
    108 116 124 136 148 160 172 188 204 220 240 260 284 308
    336 364 396 432 468 508 552 600 652 704 768 832 896 960 1024))

(defparameter *swb-offset-short-24*
  #(0 4 8 12 16 20 24 28 36 44 52 64 76 92 108 128))

(defparameter *swb-offset-long-16*
  #(0 8 16 24 32 40 48 56 64 72 80 88 100 112 124 136 148
    160 172 184 196 212 228 244 260 280 300 320 344 368 396
    424 456 492 532 572 616 664 716 772 832 896 960 1024))

(defparameter *swb-offset-short-16*
  #(0 4 8 12 16 20 24 28 32 40 48 60 72 88 108 128))

(defparameter *swb-offset-long-8*
  #(0 12 24 36 48 60 72 84 96 108 120 132 144 156 172
    188 204 220 236 252 268 288 308 328 348 372 396 420
    448 476 508 544 580 620 664 712 764 820 880 944 1024))

(defparameter *swb-offset-short-8*
  #(0 4 8 12 16 20 24 28 36 44 52 60 72 88 108 128))

(defun get-swb-offset-long-window (sampling-frequency-index)
  (ecase (get-sampling-frequeny sampling-frequency-index)
    ((96000 88200) *swb-offset-long-96*)
    ((64000)       *swb-offset-long-64*)
    ((48000 44100) *swb-offset-long-48*)
    ((32000)       *swb-offset-long-32*)
    ((24000 22050) *swb-offset-long-24*)
    ((16000 12000 11025) *swb-offset-long-16*)
    ((8000)        *swb-offset-long-8*)))

(defun get-swb-offset-short-window (sampling-frequency-index)
  (ecase (get-sampling-frequeny sampling-frequency-index)
    ((96000 88200) *swb-offset-short-96*)
    ((64000)       *swb-offset-short-64*)
    ((48000 44100) *swb-offset-short-48*)
    ((32000)       *swb-offset-short-32*)
    ((24000 22050) *swb-offset-short-24*)
    ((16000 12000 11025) *swb-offset-short-16*)
    ((8000)        *swb-offset-short-8*)))
