;;;; package.lisp

(defpackage #:sea-pie
  (:documentation "Reading and parsing text files")
  (:use #:cl #:series)
  (:export #:read-word
           #:gather-word-frequencies
           #:gather-word-frequencies-from-file))

