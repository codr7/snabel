(in-package snabl)

(defvar *debug* nil)

(defun repl (&key (in *standard-input*) (out *standard-output*))
  (flet ((fmt (spec &rest args)
           (apply #'format out spec args)
           (finish-output out)))
    (fmt "Snabl v~a~%" *version*)
    (fmt "Press Return twice to evaluate.~%")
    (fmt "May the source be with you!~%~%")

    (with-output-to-string (buf)
      (labels ((get-line ()
		 (fmt "  ")
		 (let ((lin (read-line in nil)))
		   (when lin
		     (if (string= lin "")
			 (let ((fin (make-string-input-stream (get-output-stream-string buf)))
			       (start-pc *pc*)
			       (pos (new-pos "repl")))
			   (handler-case
			       (progn
				 (labels ((get-forms (out)
					    (let ((f (parse-form fin pos)))
					      (if f
						  (get-forms (cons f out))
						  (nreverse out)))))
				   (emit-forms (get-forms nil)))
				 (vm-eval :start-pc start-pc))
			     (error (e)
			       (when *debug*
				 (error e))
			       (format t "~a~%" e)))
			   (dump-stack)
			   (terpri out))
			 (write-string lin buf))
		     (get-line)))))
	(get-line)))))
