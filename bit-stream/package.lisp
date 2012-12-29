(defpackage laac.bit-stream
  (:use :common-lisp)
  (:export with-oop-like

           bit-stream
           make
           eos?
           read-bits

           drop-unaligned-bits
           read-bytes))
(in-package :laac.bit-stream)
