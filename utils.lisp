(defpackage utils
  (:use cl)
  (:export sym ws?))

(in-package utils)

(defun sym (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	      (princ (if (stringp a) (string-upcase a) a) out)))))

(defun ws? (c)
  (when (or (char= c #\space) (char= c #\tab) (char= c #\newline))
    c))
