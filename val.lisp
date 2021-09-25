(in-package clvm)

(defstruct (val (:conc-name))
  (vm-type (error "Missing type") :type vm-type)
  (data (error "Missing data") :type t))

(defun new-val (vm-type data)
  (make-val :vm-type vm-type :data data))

(defun copy (src) (copy-structure src))

(defun clone (src)
  (let ((dst (copy src)))
    (setf (data dst) (funcall (val-clone (vm-type src)) (data dst)))
    dst))

(defun is-true? (val)
  (funcall (val-is-true? (vm-type val)) (data val)))

(defmethod dump ((val val) &key (out *standard-output*))
  (funcall (val-dump (vm-type val)) (data val) out))
