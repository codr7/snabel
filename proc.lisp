(in-package snabl)

(defclass proc ()
  ((scope :initform nil)
   (regs :initform (make-array *max-reg-count* :element-type 'val) :reader regs)
   (stack :initform (make-array 0 :element-type 'val :fill-pointer 0))))

(defun push-proc (proc &key (vm *vm*))
  (with-slots (procs proc-cache) vm
    (push proc procs)
    (setf proc-cache proc)))

(defun pop-proc (&key (vm *vm*))
  (with-slots (procs proc-cache) vm
    (pop procs)
    (let ((n (length procs)))
      (setf proc-cache (if n (aref procs (1- n) nil))))))

