(in-package #:t-sea-pie)

(in-suite test-all)
(defsuite* test-word-frequencies)

(defparameter *words* "
   Pol. How say you by that? Still harping on my daughter:
yet he knew me not at first; he said I was a Fishmonger:
he is farre gone, farre gone: and truly in my youth,
I suffred much extreamity for loue: very neere this. Ile
speake to him againe. What do you read my Lord?
  Ham. Words, words, words")

(deftest test-frequency-counts ()
  (with-input-from-string (stream *words*)
    (let ((freqs
           (gather-word-frequencies stream)))
      (are (= 1 (gethash 'Fishmonger freqs))
           (= 2 (gethash 'farre freqs))
           (= 3 (gethash 'words freqs))))))

(deftest test-total-sum ()
  (with-input-from-string (stream "this is the day this is the hour")
    (multiple-value-bind (freqs sum)
        (gather-word-frequencies stream)
      (is (= 8 sum)))))

(deftest test-different-normalizer ()
  (with-input-from-string (stream *words*)
    (let ((freqs
           (gather-word-frequencies stream :normalizer #'identity :test 'equal)))
      (are (null (gethash 'Fishmonger freqs))
           (= 1 (gethash "Fishmonger" freqs))))))
