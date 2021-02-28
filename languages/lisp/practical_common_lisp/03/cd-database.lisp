(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-integer (prompt)
  (or (parse-integer (prompt-read prompt) :junk-allowed t) 0))

(defun prompt-string (prompt)
  (prompt-read prompt))

(defun prompt-bool (prompt)
  (y-or-n-p prompt))

(defun prompt-for-cd ()
  (make-cd
    (prompt-string "Title")
    (prompt-string "Artist")
    (prompt-integer "Rating")
    (prompt-bool "Ripped [y/n]")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
    (if (not (prompt-bool "Enter Another CD? [y/n]")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
  (with-standard-io-syntax
    (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))
