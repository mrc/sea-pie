;;;; package.lisp

(defpackage #:t-sea-pie
  (:use #:cl #:sea-pie #:stefil #:mrc-stefil-extras)
  (:export #:test-all))

(in-package #:t-sea-pie)
(in-root-suite)
(defsuite test-all)
