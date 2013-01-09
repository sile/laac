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

               ;; bitstream
               (:file "bit-stream/package")
               (:file "bit-stream/bit-stream")

               ;; huffman
               (:file "huffman/package")
               (:file "huffman/table")
               (:file "huffman/huffman")
                   
               ;; adts
               (:file "adts/package")
               (:file "adts/syntax")
               (:file "adts/adts")

               ;; aac
               (:file "aac/package")
               (:file "aac/table")
               (:file "aac/syntax")
               (:file "aac/parse")
               (:file "aac/fft-table")
               (:file "aac/fft") ; XXX: move to util
               (:file "aac/mdct-table")
               (:file "aac/mdct")
               (:file "aac/filterbank")
               (:file "aac/decode")
               (:file "aac/aac")

               (:file "laac")))
