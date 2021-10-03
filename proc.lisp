(in-package snabl)

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(defclass proc ()
  ((scope :initform nil)
   (regs :initform (make-array *max-reg-count* :initial-element nil) :reader regs)
   (stack :initarg :stack)))

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
  (with-slots (regs) *proc*
    (aref regs (the integer reg))))

(defun (setf get-reg) (val reg)
  (with-slots (regs) *proc*
    (setf (aref regs (the integer reg)) val)))
