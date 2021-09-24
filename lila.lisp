(defpackage lila
  (:use cl)
  (:import-from clvm column emit eval id-form line parse-int parse-ws pc pos)
  (:import-from utils kw ws?)
  (:export parse-form parse-id))

(in-package lila)

(define-symbol-macro *version* 1)

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
      (id-form (kw s) :pos fpos))))

(defun parse-form (in vm pos)
  (or (parse-ws in pos)
      (parse-int in vm pos)
      (parse-id in pos)))

(defun repl (&key (in *standard-input*) (out *standard-output*))
  (flet ((fmt (spec &rest args)
           (apply #'format out spec args)
           (finish-output out)))
    (fmt "lila v~a~%~%" *version*)
    (fmt "Press Return twice to evaluate forms, Ctrl+C exits.~%")
    (fmt "May the source be with you!~%~%")

    (let ((pos (new-pos "repl")))
      (with-output-to-string (buf)
	(labels ((get-line ()
		   (fmt "  ")
		   (let ((lin (read-line in nil)))
		     (when lin
		       (if (string= lin "")
			   (let ((fin (make-string-input-stream (get-output-stream-string buf)))
				 (start-pc (pc)))
			     (labels ((get-form ()
					(let ((f (parse-form fin pos)))
					  (when f
					    (emit f)
					    (read-form)))))
			       (get-form))
			     (vm-eval :pc start-pc))
			   (write-string lin buf))
		       (get-line)))))
	  (get-line))))))
