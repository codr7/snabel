(in-package snabl)

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(defun vm-push (val)
  (declare (type val val))
  (vector-push-extend val *stack*))

(defun vm-push-new (vm-type data)
  (vm-push (new-val (the vm-type vm-type) data)))

(defun vm-peek (&key (offset 0))
  (let ((n (length *stack*)))
    (when (< offset n)
      (aref *stack* (- n offset 1)))))

(defun vm-pop (&key (offset 0))
  (declare (type integer offset))
  (when (< offset (length *stack*))
    (if (zerop offset)
	(vector-pop *stack*)
	(delete-at *stack* (- (length *stack*) offset 1)))))

(defun drop (&optional (count 1))
  (declare (type integer count))
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

(defun stack-copy (in &key (start 0) end)
  (declare (type sequence in))

  (let* ((n (- (or end (length in)) start))
	 (out (make-array n :element-type 'val :fill-pointer n)))
    (declare (type integer n) (type array out))
    
    (dotimes (i n)
      (setf (aref out i) (aref in (+ start i))))

    out))
