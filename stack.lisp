(in-package snabl)

(defun vm-push (val)
  (vector-push-extend val *stack*))

(defun vm-push-new (vm-type data)
  (vm-push (new-val vm-type data)))

(defun vm-pop ()
  (unless (zerop (length *stack*))
    (vector-pop *stack*)))

(defun drop (&optional (count 1))
  (let ((len (length *stack*)))
    (when (<= count len)
      (decf (fill-pointer *stack*) count)
      t)))

(defun dump-stack (&key (out *standard-output*))
  (princ #\[ out)
  (dotimes (i (length *stack*))
    (unless (zerop i)
      (princ #\space out))
    (dump (aref *stack* i) :out out))
  (princ #\] out))
