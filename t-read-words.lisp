(in-package #:t-sea-pie)

(in-suite test-all)
(defsuite* test-read-words)

(defparameter *monologue* "
    Now is the winter of our discontent
    Made glorious summer by this sun of York;
    And all the clouds that lour'd upon our house
    In the deep bosom of the ocean buried.")

(deftest fetch-one-word ()
  (with-input-from-string (stream "Eskimo!")
    (are (equal "Eskimo" (read-word stream))
         (null (read-word stream)))))

(deftest fetch-word-with-contraction ()
  (with-input-from-string (stream "lour'd")
    (is (string= "lour'd" (read-word stream)))))

(deftest fetch-last-word ()
  (with-input-from-string (stream "Robot Monster")
    (are (equal "Robot" (read-word stream))
         (equal "Monster" (read-word stream))
         (null (read-word stream)))))

(deftest ignore-runs-of-punctuation ()
  (with-input-from-string (stream "Robot   !!!!   Monster!!! ")
    (are (equal "Robot" (read-word stream))
         (equal "Monster" (read-word stream))
         (null (read-word stream)))))

(deftest fetch-words-across-newline ()
  (with-input-from-string (stream "Now is the winter of our discontent
Made glorious summer by this sun of York;
And all the clouds that lour'd upon our house
In the deep bosom of the ocean buried.")
    (let ((words
           (loop for w = (read-word stream)
              while w
              collect w)))
      (is (equal words '("Now" "is" "the" "winter" "of" "our" "discontent"
                         "Made" "glorious" "summer" "by" "this" "sun" "of" "York"
                         "And" "all" "the" "clouds" "that" "lour'd" "upon" "our" "house"
                         "In" "the" "deep" "bosom" "of" "the" "ocean" "buried"))))))
