(in-package clvm)

(defun lit-tests ()
  (let ((*vm* (make-vm)))
    (emit (lit (make-pos) (int-type (abc-lib *vm*)) 42))
    (vm-eval)
    (assert (= (val-data (vm-pop)) 42))))

(defun label-tests ()
  (let* ((*vm* (make-vm))
	 (p (make-pos))
	 (f (id p :foo)))
    (emit (make-goto-op :form f :label :foo))
    (emit (make-push-op :form f :val (val (int-type (abc-lib *vm*)) 1)))
    (emit (make-label-op :form f :name :foo))
    (emit (make-push-op :form f :val (val (int-type (abc-lib *vm*)) 2)))
    (vm-eval)
    (assert (= (val-data (vm-pop)) 2))
    (assert (null (vm-pop)))))

(defun all-tests ()
  (lit-tests)
  (label-tests))
  
