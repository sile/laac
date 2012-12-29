(in-package :asdf)

(defsystem laac
  :name "laac"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "An AAC decoder"
  
  :serial t
  :components ((:file "package")
               (:file "laac")))
