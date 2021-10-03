(in-package snabl)

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(define-symbol-macro time-units-per-ms (/ internal-time-units-per-second 1000))

(defun all? (seq pred)
  (not (find-if-not pred seq)))

(defun char-digit (c)
  (- (char-code c) (char-code #\0)))

(defun copy-vector (in)
  (make-array (length in) :initial-contents in :fill-pointer (length in)))

(defun delete-at (seq i)
  (let ((elt (aref seq i))
	(slide (subseq seq (1+ i)))
	(count (1- (fill-pointer seq))))
    (replace seq slide :start1 i)
    (adjust-array seq count :fill-pointer count)
    elt))

(defmacro dohash ((key val tbl) &body body)
  (let ((i (gensym)) (more? (gensym)) (rec (gensym)))
    `(with-hash-table-iterator (,i ,tbl)
       (labels ((,rec ()
		  (multiple-value-bind (,more? ,key ,val) (,i)
		    (when ,more?
		      (let ()
			(declare (ignorable ,key ,val))
			,@body
			(,rec))))))
	 (,rec)))))

(defmacro dovector ((val vec) &body body)
  (let ((i (gensym)))
    `(dotimes (,i (length ,vec))
       (let ((,val (aref ,vec ,i)))
	 ,@body))))

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
	      (princ a out)))))

(defun ws? (c)
  (when (or (char= c #\space) (char= c #\tab) (char= c #\newline))
    c))
