;;;; sea-pie.lisp

(in-package #:sea-pie)

(defun word-char-p (c)
  "True if c is a word char, by an ever mutating definition."
  (or (alpha-char-p c)
      (char= #\' c)))

(defun read-word (stream)
  "Read a word from `STREAM', possibly after swallowing non-word
characters."
  (let ((current-word (make-array 10 :fill-pointer 0 :adjustable t :element-type 'character)))
    (labels ((see (c)
               (vector-push-extend c current-word))
             (swallow-non-word ()
               (loop for c = (read-char stream nil)
                  thereis (null c)
                  until (word-char-p c)
                  finally (unread-char c stream))))
      (swallow-non-word)
      (loop for c = (read-char stream nil)
         thereis (null c)
         while (word-char-p c)
         do (see c)))
    (if (> (length current-word) 0)
        (concatenate 'string current-word)
        nil)))

(defun upcase-and-intern-string (string)
  "Return a symbol made out of the uppercased string."
  (intern (string-upcase string)))

(defun gather-ngrams (order generator collector)
  "Call `GENERATOR', until it returns nil. For each ngram of length
`ORDER', call `COLLECTOR'. Returns the number of ngrams found."
  (let (next-ngram (ngram-count 0))
    (labels ((see (c)
               (push c next-ngram)
               (when (= order (length next-ngram))
                 (funcall collector (reverse next-ngram))
                 (setf next-ngram (nbutlast next-ngram))
                 (incf ngram-count))))
      (loop for c = (funcall generator)
         while c
         do (see c)
         finally (return ngram-count)))))

(defun gather-ngram-frequencies (order generator normalizer &key (test 'eql))
  "Build a frequency table of ngrams of length `ORDER`. Calls
`GENERATOR' to fetch each element, which should signal the end of
input by returning nil. Returns the frequency table and the number of
ngrams found."
  (let ((ht (make-hash-table :test test)))
    (labels ((collect-ngram (ngram)
               (incf (gethash (funcall normalizer ngram) ht 0))))
      (values ht (gather-ngrams order generator #'collect-ngram)))))

(defun gather-word-frequencies (stream &key normalizer (test 'eql))
  "Read all words from `STREAM'. Returns a hash table mapping
words (normalized by `NORMALIZER', which defaults to converting the
uppercased words to symbols) to a count of the number of
occurrences. The second returned value is the total number of words
seen, including duplicates."
  (unless normalizer
    (setf normalizer #'upcase-and-intern-string))
  (gather-ngram-frequencies 1
                            (lambda () (read-word stream))
                            (lambda (ngram) (funcall normalizer (car ngram)))
                            :test test))

(defun gather-letter-ngram-frequencies-from-stream (order stream)
  "Returns a frequency table of word-character ngrams of length
`ORDER' from the stream."
  (labels ((generate-letter ()
             (let ((ch (read-char stream nil)))
               (cond
                 ((null ch) nil)
                 ((word-char-p ch) (char-upcase ch))
                 (t #\Space))))
           (chars-to-string (chars)
             (concatenate 'string chars)))
    (gather-ngram-frequencies order
                              #'generate-letter
                              #'chars-to-string
                              :test 'equal)))

(defun gather-word-ngram-frequencies-from-stream (order stream)
  "Returns a frequency table of word tuples of length `ORDER' from the
stream."
  (gather-ngram-frequencies order
                            (lambda () (read-word stream))
                            (lambda (ngram) (mapcar #'upcase-and-intern-string ngram))
                            :test 'equal))
