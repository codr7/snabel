(in-package snabl)

(defun parse-ws (in pos)
  (labels ((rec ()
	     (let ((c (read-char in nil)))
	       (cond
		 ((null c))
		 ((char= c #\newline)
		  (incf (line pos))
		  (setf (column pos) *min-column*)
		  (rec))
		 ((or (char= c #\space) (char= c #\tab))
		  (incf (column pos))
		  (rec))
		 (t (unread-char c in))))))
    (rec))
  nil)

(defun parse-prefix (in pos pc)
  (let ((c (read-char in nil)))
    (if (eq c pc)
	(let ((fpos (clone pos)))
	  (incf (column pos))
	  fpos)
	
	(when c
	  (unread-char c in)
	  nil))))

(defun parse-cte (in pos)
  (let ((fpos (parse-prefix in pos #\#)))
    (unless fpos
      (return-from parse-cte))

    (let ((expr (parse-form in pos)))
      (unless expr
	(e-parse pos "Missing CTE form"))
      (new-cte-form expr :pos fpos))))

(defun parse-group (in pos)
  (let ((fpos (parse-prefix in pos #\()))
    (unless fpos
      (return-from parse-group))
    (let (body)
      (labels ((rec ()
		 (parse-ws in pos)
		 (let ((c (read-char in nil)))
		   (cond
		     ((null c) (e-parse pos "Open group"))
		     ((char= c #\))
		      (incf (column pos))
		      (return-from rec))
		     (t (unread-char c in))))
		 (let ((f (parse-form in pos)))
		   (when f
		     (push f body)
		     (rec)))))
	(rec))
      (new-group-form (nreverse body) :pos fpos))))

(defun parse-id (in pos)
  (let ((fpos (clone pos))
	(s (with-output-to-string (out)
	     (labels ((rec ()
			(let ((c (read-char in nil)))
			  (when c
			    (if (or (ws? c)
				    (char= c #\.)
				    (char= c #\() (char= c #\))
				    (char= c #\{) (char= c #\}))
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
  (let ((fpos (parse-prefix in pos #\$)))
    (unless fpos
      (return-from parse-lisp))
    
    (let ((ipos (file-position in))
	  (f (read in nil)))
      (unless f
	(e-parse pos "Missing Lisp form"))
      (incf (column pos) (- (file-position in) ipos))
      (new-lisp-form (eval `(lambda () ,f)) :pos fpos))))

(defun parse-nop (in pos)
  (let ((fpos (parse-prefix in pos #\_)))
    (unless fpos
      (return-from parse-nop))

    (new-nop-form :pos fpos)))

(defun parse-scope (in pos)
  (let ((fpos (parse-prefix in pos #\{)))
    (unless fpos
      (return-from parse-scope))

    (let (body)
      (labels ((rec ()
		 (parse-ws in pos)
		 (let ((c (read-char in nil)))
		   (cond
		     ((null c) (e-parse pos "Open scope"))
		     ((char= c #\})
		      (incf (column pos))
		      (return-from rec))
		     (t (unread-char c in))))
		 (let ((f (parse-form in pos)))
		   (when f
		     (push f body)
		     (rec)))))
	(rec))
      (new-scope-form (nreverse body) :pos fpos))))

(defun parse-quote (in pos)
  (let ((fpos (parse-prefix in pos #\')))
    (unless fpos
      (return-from parse-quote))
    (let ((f (parse-form in pos)))
      (unless f
	(e-parse pos "Missing quoted expression"))
      (new-quote-form f :pos fpos))))

(defun parse-form (in pos)
  (let ((f (or (parse-ws in pos)
	       (parse-int in pos)
	       (parse-nop in pos)
	       (parse-group in pos)
	       (parse-scope in pos)
	       (parse-quote in pos)
	       (parse-cte in pos)
	       (parse-lisp in pos)
	       (parse-id in pos))))
    (when f
      (let ((c (read-char in nil)))
	(if c
	    (case c
	      (#\.
	       (new-dot-form f :pos (pos f)))
	      (otherwise
	       (unread-char c in)
	       f))
	    f)))))
