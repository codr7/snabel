(defpackage utils
  (:use cl)
  (:export sym))

(in-package utils)

(defun sym (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	      (princ (if (stringp a) (string-upcase a) a) out)))))
