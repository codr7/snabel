(defpackage lila
  (:use cl)
  (:import-from clvm id-form parse-int parse-ws)
  (:import-from utils ws?)
  (:export parse-form parse-id))

(in-package lila)

(defun parse-id (in pos)
  (let ((fpos pos)
	(s (with-output-to-string (out)
	     (labels ((rec ()
			(let ((c (read-char in nil)))
			  (when c
			    (if (ws? c)
				(unread-char c in)
				(progn
				  (incf (column pos))
				  (write-char c out)
				  (rec)))))))
	       (rec)))))
    (unless (zerop (length s))
      (id-form s :pos fpos))))

(defun parse-form (in vm pos)
  (or (parse-ws in pos)
      (parse-int in vm pos)
      (parse-id in pos)))

