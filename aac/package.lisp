(defpackage laac.aac
  (:use :common-lisp :laac.util)
  (:export parse-raw-data-block
           decode
           decode-frame))
(in-package :laac.aac)
