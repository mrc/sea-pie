;;;; package.lisp

(defpackage #:t-sea-pie
  (:use #:cl #:sea-pie #:stefil #:rep-theatre)
  (:export #:test-all))

(in-package #:t-sea-pie)
(in-root-suite)
(defsuite test-all)
