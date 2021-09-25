(in-package clvm)

(defstruct (val (:conc-name))
  (vm-type (error "Missing type") :type vm-type)
  (data (error "Missing data") :type t))

(defun new-val (vm-type data)
  (make-val :vm-type vm-type :data data))

(defmethod dump ((val val) &key (out *standard-output*))
  (funcall (val-dump (vm-type val)) (data val) out))

(defmethod is-true? ((val val))
  (funcall (val-is-true? (vm-type val)) (data val)))
