(in-package snabl)

(defclass proc ()
  ((scope :initform nil)
   (regs :initform (make-array *max-reg-count* :initial-element nil) :reader regs)
   (stack :initform (make-array 0 :element-type 'val :fill-pointer 0))))

(defun push-proc (proc &key (vm *vm*))
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
    (aref regs reg)))

(defun (setf get-reg) (val reg)
  (with-slots (regs) *proc*
    (setf (aref regs reg) val)))
