(defpackage utils
  (:use cl)
  (:export char-digit kw reverse-vector sym ws?))

(in-package utils)

(defun char-digit (c)
  (- (char-code c) (char-code #\0)))

(defun kw (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	      (princ (if (stringp a) (string-upcase a) a) out)))
	  :keyword))

(defun reverse-vector (vec &optional (start 0))
  (dotimes (i (floor (- (length vec) start) 2))
    (let ((tmp (aref vec (+ start i))))
      (setf (aref vec (+ start i)) (aref vec (- (length vec) i 1))
	    (aref vec (- (length vec) i 1)) tmp))))

(defun sym (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	      (princ (if (stringp a) (string-upcase a) a) out)))))

(defun ws? (c)
  (when (or (char= c #\space) (char= c #\tab) (char= c #\newline))
    c))
