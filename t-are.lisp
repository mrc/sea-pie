(in-package #:t-sea-pie)

(defmacro are (test &rest tests)
  "Evaluate a list of tests with is"
  `(progn
     (is ,test)
     (unless (null ',tests)
       (are ,@tests))))
