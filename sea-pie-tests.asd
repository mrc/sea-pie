;;;; sea-pie-tests.asd

(asdf:defsystem #:sea-pie-tests
  :description "Tests for sea-pie."
  :author "Matt Curtis"
  :licence "BSD-new"
  :name "sea-pie-tests"
  :version "0.1"
  :serial t
  :depends-on (#:cl-utilities
               #:sea-pie
               #:rep-theatre
               #:stefil)
  :components ((:file "t-package")
               (:file "t-read-words")
               (:file "t-ngram-frequencies")
               (:file "t-word-frequencies")))

