(in-package clvm)
		 
(defclass vm ()
  ((abc-lib :initform (make-instance 'abc-lib) :reader abc-lib)
   (code :initform (make-array 0 :element-type 'op :fill-pointer 0) :reader code)
   (stack :initform (make-array 0 :element-type 'val :fill-pointer 0) :reader stack)))

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
