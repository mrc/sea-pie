;;;; sea-pie.asd

(asdf:defsystem #:sea-pie
  :description "Tools for processing a text file containing words."
  :author "Matt Curtis"
  :licence "BSD-new"
  :name "sea-pie"
  :version "0.1"
  :serial t
  :depends-on (#:cl-utilities
               #:rep-theatre)
  :components ((:file "package")
               (:file "sea-pie")))

