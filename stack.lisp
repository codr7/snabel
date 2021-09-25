(in-package snabl)

(defun vm-push (val)
  (vector-push-extend val (stack *vm*)))

(defun vm-push-new (vm-type data)
  (vm-push (new-val vm-type data)))

(defun vm-pop ()
  (unless (zerop (length (stack *vm*)))
    (vector-pop (stack *vm*))))

(defun drop (&optional (count 1))
  (dotimes (i count)
    (unless (vm-pop)
      (return-from drop nil)))
  t)
     
(defun dump-stack (&key (out *standard-output*))
  (princ #\[ out)
  (let ((stack (stack *vm*)))
    (dotimes (i (length stack))
      (unless (zerop i)
	(princ #\space out))
      (dump (aref stack i) :out out)))
  (princ #\] out))
