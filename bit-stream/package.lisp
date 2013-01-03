(defpackage laac.bit-stream
  (:use :common-lisp)
  (:export bit-stream

           make
           make-from-bytes
           eos?
           read-bits
           read-bool
 
           drop-unaligned-bits
           read-bytes))
(in-package :laac.bit-stream)
