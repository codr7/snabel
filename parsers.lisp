(in-package snabl)

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

(defun parse-prefix (in pos pc)
  (let ((c (read-char in nil)))
    (if (eq c pc)
	(progn
	  (incf (column pos))
	  t)
	(when c
	  (unread-char c in)
	  nil))))

(defun parse-cte (in pos)
  (let ((fpos (clone pos)))
    (unless (parse-prefix in pos #\#)
      (return-from parse-cte))
    (let ((expr (parse-form in pos)))
      (unless expr
	(error "Missing CTE form"))
      (new-cte-form expr :pos fpos))))

(defun parse-id (in pos)
  (let ((fpos (clone pos))
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
  (let ((fpos (clone pos))
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
	(new-lit-form (new-val (int-type *abc-lib*) out) :pos fpos)))))

(defun parse-lisp (in pos)
  (let ((fpos (clone pos)))
    (unless (parse-prefix in pos #\$)
      (return-from parse-lisp))
    
    (let ((ipos (file-position in))
	  (f (read in nil)))
      (unless f
	(error "Missing Lisp form"))
      (incf (column pos) (- (file-position in) ipos))
      (new-lisp-form (eval `(lambda () ,f)) :pos fpos))))

(defun parse-nop (in pos)
  (let ((fpos (clone pos)))
    (unless (parse-prefix in pos #\_)
      (return-from parse-nop))
    (new-nop-form :pos fpos)))

(defun parse-form (in pos)
  (or (parse-ws in pos)
      (parse-int in pos)
      (parse-nop in pos)
      (parse-cte in pos)
      (parse-lisp in pos)
      (parse-id in pos)))
