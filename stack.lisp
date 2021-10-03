(in-package snabl)

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(defun vm-push (val)
  (vector-push-extend val *stack*))

(defun vm-push-new (vm-type data)
  (vm-push (new-val vm-type data)))

(defun vm-peek ()
  (let ((n (length *stack*)))
    (unless (zerop n)
      (aref *stack* (1- n)))))

(defun vm-pop (&key (offset 0))
  (when (< offset (length *stack*))
    (if (zerop offset)
	(vector-pop *stack*)
	(delete-at *stack* (- (length *stack*) offset 1)))))

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
