(in-package clvm)

(defun lit-tests ()
  (let ((*vm* (make-vm)))
    (emit (lit-form (int-type (abc-lib *vm*)) 42))
    (vm-eval)
    (assert (= (data (vm-pop)) 42))))

(defun label-tests ()
  (let* ((*vm* (make-vm))
	 (int (int-type (abc-lib *vm*))))
    (emit (goto-op :foo))
    (emit (push-op int 1))
    (emit (label-op :foo))
    (emit (push-op int 2))
    (vm-eval)
    (assert (= (data (vm-pop)) 2))
    (assert (null (vm-pop)))))

(defun all-tests ()
  (lit-tests)
  (label-tests))
  
