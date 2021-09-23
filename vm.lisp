(in-package clvm)
		 
(defstruct (vm (:conc-name))
  (abc-lib (make-abc-lib :name :abc) :type abc-lib)
  (code (make-array 0 :element-type 'op :fill-pointer 0))
  (stack (make-array 0 :element-type 'val :fill-pointer 0)))

(defmethod emit ((op op))
  (vector-push-extend op (code *vm*)))

(defun vm-eval (&key (pc 0))
  (dotimes (i (- (length (code *vm*)) pc))
    (let ((op (aref (code *vm*) (+ pc i))))
      (eval (emit-lisp op)))))
    
