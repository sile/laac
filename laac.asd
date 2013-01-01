(in-package :asdf)

(defsystem laac
  :name "laac"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "An AAC decoder"

  :depends-on (:flexi-streams)
  :serial t
  :components ((:file "package")

               ;; util
               (:file "util/package")
               (:file "util/util")

			   ;; huffman
			   (:file "huffman/package")
			   (:file "huffman/huffman")
                   
               ;; bitstream
               (:file "bit-stream/package")
               (:file "bit-stream/bit-stream")

               ;; adts
               (:file "adts/package")
               (:file "adts/syntax")
               (:file "adts/adts")

               ;; aac
               (:file "aac/package")
               (:file "aac/syntax")
               (:file "aac/parse")
               (:file "aac/aac")

               (:file "laac")))
