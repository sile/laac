(in-package :asdf)

(defsystem laac
  :name "laac"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "An AAC decoder"
  
  :serial t
  :components ((:file "package")

               ;; bitstream
               (:file "bit-stream/package")
               (:file "bit-stream/bit-stream")

               ;; adts
               (:file "adts/package")
               (:file "adts/syntax")
               (:file "adts/adts")

               (:file "laac")))
