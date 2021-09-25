(in-package lila)

(defun parse-ws (in pos)
  (labels ((rec ()
	     (let ((c (read-char in nil)))
	       (when c
		 (case c
		   (#\newline
		    (incf (line pos))
		    (setf (column pos) *min-column*)
		    (rec))
		   ((#\space #\tab)
		    (incf (column pos))
		    (rec))
		   (otherwise
		    (unread-char c in)))))))
    (rec))
  nil)

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
      (new-id-form (kw s) :pos fpos))))

(defun parse-int (in pos)
  (let ((fpos pos)
	(out 0))
    (labels ((rec (result)
	       (let ((c (read-char in nil)))
		 (if c
		   (if (digit-char-p c)
		       (progn
			 (incf (column pos))
			 (setf out (+ (* out 10) (char-digit c)))
			 (rec t))
		       (progn
			 (unread-char c in)
			 result))
		   result))))
      (when (rec nil)
	(new-lit-form (int-type (abc-lib *vm*)) out :pos fpos)))))

(defun parse-form (in pos)
  (or (parse-ws in pos)
      (parse-int in pos)
      (parse-id in pos)))
