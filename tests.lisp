(in-package snabl)

(defun lit-tests ()
  (let ((*vm* (new-vm)))
    (form-emit (new-lit-form (new-val (int-type *abc-lib*) 42) nil))
    (vm-eval)
    (assert (= (data (vm-pop)) 42))))

(defun label-tests ()
  (let* ((*vm* (new-vm))
	 (int (int-type *abc-lib*)))
    (emit-op (new-goto-op :foo))
    (emit-op (new-push-op (new-val int 1)))
    (emit-op (new-label-op :foo))
    (emit-op (new-push-op (new-val int 2)))
    (vm-eval)
    (assert (= (data (vm-pop)) 2))
    (assert (null (vm-pop)))))

(defun branch-tests ()
  (let* ((*vm* (new-vm))
	 (int (int-type *abc-lib*)))
    (emit-op (new-push-op (new-val int 0)))
    (emit-op (new-branch-op :false))
    (emit-op (new-push-op (new-val int 1)))
    (emit-op (new-goto-op :end))
    (emit-op (new-label-op :false))
    (emit-op (new-push-op (new-val int 2)))
    (emit-op (new-label-op :end))
    (vm-eval)
    (assert (= (data (vm-pop)) 2))
    (assert (null (vm-pop)))))

(defun all-tests ()
  (lit-tests)
  (label-tests)
  (branch-tests))
