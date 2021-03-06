(in-package snabl)

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(defstruct proc
  (scope nil)
  (regs (make-array *max-reg-count* :initial-element nil))
  (stack nil))

(defun push-proc (proc &key (vm *vm*))
  (declare (type proc proc))
  
  (with-slots (procs proc-cache) vm
    (vector-push-extend proc procs)
    (setf proc-cache proc)))

(defun pop-proc (&key (vm *vm*))
  (with-slots (procs proc-cache) vm
    (vector-pop procs)
    (let ((n (length procs)))
      (setf proc-cache (unless (zerop n) (aref procs (1- n)))))))

(defun get-reg (reg)
  (aref *regs* (the integer reg)))

(defun (setf get-reg) (val reg)
  (setf (aref *regs* (the integer reg)) val))
