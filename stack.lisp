(in-package clvm)

(defun vm-push (type data)
  (vector-push-extend (make-val :vm-type type :data data) (stack *vm*)))

(defun vm-pop ()
  (unless (zerop (length (stack *vm*)))
    (vector-pop (stack *vm*))))
