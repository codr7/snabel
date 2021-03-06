(in-package snabl)

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(defstruct (val (:conc-name))
  (vm-type (error "Missing type") :type vm-type)
  (data (error "Missing data") :type t))

(defun new-val (vm-type data)
  (make-val :vm-type vm-type :data data))

(defmethod compare (x y)
  (cond
    ((< x y) :lt)
    ((> x y) :gt)
    (t :eq)))

(defmethod compare ((x val) (y val))
  (if (eq (vm-type x) (vm-type y))
      (funcall (val-compare (vm-type x)) (data x) (data y))
      (compare (id (vm-type x)) (id (vm-type y)))))

(defun copy (src) (copy-structure src))

(defmethod clone ((src val))
  (let ((dst (copy src)))
    (setf (data dst) (funcall (val-clone (vm-type src)) (data dst)))
    dst))

(defmethod dump ((val val) &key (out *standard-output*))
  (funcall (val-dump (vm-type val)) (data val) out))

(defun is (x y)
  (and (eq (vm-type x) (vm-type y)) (funcall (val-is (vm-type x)) (data x) (data y))))
		    
(defmethod print-object ((self val) out)
  (dump self :out out))

(defun true? (val)
  (funcall (val-true? (vm-type val)) (data val)))
