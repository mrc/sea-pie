;;;; sea-pie.lisp

(in-package #:sea-pie)

(defun read-word (stream)
  "Read a word from `STREAM', possibly after swallowing non-word
characters."
  (let (current-word)
    (labels ((word-char-p (c)
               (or (alpha-char-p c)
                   (char= #\' c)))
             (see (c)
               (push c current-word))
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
    (if current-word
        (concatenate 'string (nreverse current-word))
        nil)))

(defun gather-word-frequencies (stream &key normalizer (test 'eql))
  "Read all words from `STREAM'. Returns a hash table mapping
words (normalized by `NORMALIZER', which defaults to converting the
uppercased words to symbols) to a count of the number of
occurrences. The second returned value is the total number of words
seen, including duplicates."
  (unless normalizer
    (setf normalizer
          (lambda (word)
            (intern (string-upcase word)))))
  (let ((ht (make-hash-table :test test)))
    (loop for w = (read-word stream)
       and count from 0
       while w
       do (incf (gethash (funcall normalizer w) ht 0))
       finally (return (values ht count)))))

(defun gather-word-frequencies-from-file (file-name)
  "Run `gather-word-frequencies' on the file `FILE-NAME'."
  (with-open-file (file file-name)
    (gather-word-frequencies file)))
