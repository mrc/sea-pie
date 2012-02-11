;;;; package.lisp

(defpackage #:sea-pie
  (:documentation "Reading and parsing text files")
  (:use #:cl)
  (:export #:read-word
           #:gather-word-frequencies
           #:gather-ngram-frequencies
           #:gather-letter-ngram-frequencies-from-stream
           #:gather-word-ngram-frequencies-from-stream
           #:word-char-p
           #:normalize-and-upcase-chars))

