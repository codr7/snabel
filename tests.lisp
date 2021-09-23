(in-package clvm)

(defun all-tests ()
  (let* ((*vm* (make-vm))
	 (p (make-pos))
	 (f (make-literal :pos p :val (val (integer-type (abc-lib *vm*)) 42))))
    (emit f)
    (vm-eval)
    
    (let ((v (vm-pop)))
      (assert (eq (val-type v) (integer-type (abc-lib *vm*))))
      (assert (= (val-data v) 42)))))
      
  
