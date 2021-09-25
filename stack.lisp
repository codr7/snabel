(in-package clvm)

(defun vm-push (vm-type data)
  (vector-push-extend (new-val vm-type data) (stack *vm*)))

(defun vm-pop ()
  (unless (zerop (length (stack *vm*)))
    (vector-pop (stack *vm*))))

(defun dump-stack (&key (out *standard-output*))
  (princ #\[ out)
  (let ((stack (stack *vm*)))
    (dotimes (i (length stack))
      (unless (zerop i)
	(princ #\space out))
      (dump (aref stack i) :out out)))
  (princ #\] out))
