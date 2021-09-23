(in-package clvm)

(defun all-tests ()
  (let* ((*vm* (make-vm))
	 (p (make-pos))
	 (f (lit p (int-type (abc-lib *vm*)) 42)))
    (emit f)
    (vm-eval)
    
    (let ((v (vm-pop)))
      (assert (eq (val-type v) (int-type (abc-lib *vm*))))
      (assert (= (val-data v) 42)))))
      
  
