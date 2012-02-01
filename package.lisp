;;;; package.lisp

(defpackage #:sea-pie
  (:documentation "Reading and parsing text files")
  (:use #:cl)
  (:export #:read-word
           #:gather-word-frequencies
           #:gather-word-frequencies-from-file))

