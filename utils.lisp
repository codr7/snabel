(defpackage utils
  (:use cl)
  (:export kw sym ws?))

(in-package utils)

(defun kw (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	      (princ (if (stringp a) (string-upcase a) a) out)))
	  :keyword))

(defun sym (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	      (princ (if (stringp a) (string-upcase a) a) out)))))

(defun ws? (c)
  (when (or (char= c #\space) (char= c #\tab) (char= c #\newline))
    c))
