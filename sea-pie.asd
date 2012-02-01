;;;; sea-pie.asd

(asdf:defsystem #:sea-pie
  :description "Tools for processing a text file containing words."
  :author "Matt Curtis"
  :licence "BSD-new"
  :name "sea-pie"
  :version "0.1"
  :serial t
  :depends-on (#:cl-utilities
               #:stefil)
  :components ((:file "package")
               (:file "sea-pie")
               (:file "t-package")
               (:file "t-are")
               (:file "t-read-words")
               (:file "t-word-frequencies")))

