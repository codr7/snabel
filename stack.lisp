(in-package clvm)

(defun vm-push (vm-type data)
  (vector-push-extend (new-val vm-type data) (stack *vm*)))

(defun vm-pop ()
  (unless (zerop (length (stack *vm*)))
    (vector-pop (stack *vm*))))
