(in-package #:t-sea-pie)

(defmacro are (&rest tests)
  "Evaluate a list of tests with is."
  `(let ((result (gensym "result"))
         (test (gensym)))
     (loop :for test :in ',tests
        do (setf result (stefil:is test)))
     result))
