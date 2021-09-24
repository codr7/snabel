(in-package clvm)
		 
(defclass vm ()
  ((abc-lib :initform (make-instance 'abc-lib) :reader abc-lib)
   (code :initform (make-array 0 :element-type 'op :fill-pointer 0) :reader code)
   (main-scope :reader main-scope)
   (scope :initform nil :reader scope)
   (regs :initform (make-array *max-reg-count* :element-type 'val) :reader regs)
   (stack :initform (make-array 0 :element-type 'val :fill-pointer 0) :reader stack)))

(defun new-vm ()
  (make-instance 'vm))

(defmethod initialize-instance :after ((vm vm) &rest args &key &allow-other-keys)
  (with-slots (main-scope) vm
    (setf main-scope (begin-scope :vm vm))))

(defmethod emit ((op op))
  (vector-push-extend op (code *vm*)))

(defun vm-compile (&key (pc 0))
  (let (out)
    (dotimes (i (- (length (code *vm*)) pc))
      (let ((op (aref (code *vm*) (+ pc i))))
	(push (emit-lisp op) out)))
    
    (eval `(lambda ()
	     (tagbody
		,@(nreverse out))))))

(defun vm-eval (&key (pc 0))
  (funcall (vm-compile :pc 0)))

(defun pc ()
  (length (code *vm*)))
  
