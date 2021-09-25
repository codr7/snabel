(defpackage utils
  (:use cl)
  (:export all? char-digit dohash kw reverse-vector sym ws?))

(in-package utils)

(defun all? (seq pred)
  (not (find-if-not pred seq)))
  
(defun char-digit (c)
  (- (char-code c) (char-code #\0)))

(defmacro dohash ((key val tbl) &body body)
  (let ((i (gensym)) (ok? (gensym)))
    `(with-hash-table-iterator (,i ,tbl)
       (do () (nil)
         (multiple-value-bind (,ok? ,key ,val) (,i)
           (declare (ignorable ,key ,val))
           (unless ,ok? (return))
           ,@body)))))

(defun kw (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	      (princ a out)))
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
