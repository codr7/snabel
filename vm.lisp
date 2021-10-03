(in-package snabl)

(declaim (optimize (safety 3) (debug 0) (speed 3)))

(defclass vm ()
  ((type-count :initform 0)
   (abc-lib :initform nil)
   (math-lib :initform nil)
   (code :initform (make-array 0 :element-type 'op :fill-pointer 0) :reader code)
   (main-scope :reader main-scope)
   (procs :initform (make-array 0 :element-type 'proc :fill-pointer 0) :reader procs)
   (proc-cache :initform nil :reader proc-cache)
   (frames :initform (make-array 0 :element-type 'frame :fill-pointer 0) :reader frames)
   (frame-cache :initform nil :reader frame-cache)
   (unsafe-depth :initform 0 :reader unsafe-depth)))

(defun new-vm ()
  (make-instance 'vm))

(defmethod initialize-instance :after ((self vm) &rest args &key &allow-other-keys)
  (with-slots (main-scope proc-cache) self
    (push-proc (make-proc :stack (stack-copy nil)) :vm self)
    (setf main-scope (begin-scope :proc proc-cache))))

(defun emit-op (op)
  (vector-push-extend op (code *vm*))
  op)

(defun emit-forms (forms)
  (labels ((rec (in)
	     (when in
	       (rec (form-emit (first in) (rest in))))))
    (rec forms)))

(defun compile-main (&key (start-pc 0) (end-pc (length *code*)))  
  (let (prev-op)
    (dotimes (i (- end-pc start-pc))
      (let ((op (aref *code* (+ start-pc i))))
	(cond
	  ((and (eq (type-of prev-op) 'push-op)
		(eq (type-of op) 'dec-op))
	   (setf (dec-y op) (push-val prev-op))
	   (setf (aref *code* (1- (+ start-pc i))) *nop*))
	  ((and (eq (type-of prev-op) 'push-op)
		(eq (type-of op) 'inc-op))
	   (setf (inc-y op) (push-val prev-op))
	   (setf (aref *code* (1- (+ start-pc i))) *nop*)))
	(setf prev-op op))))
    
  (let (out)
    (dotimes (i (- end-pc start-pc))
      (let ((op (aref *code* (+ start-pc i))))
	(let ((c (emit-lisp op)))
	  (when c
	    (push c out)))))
    
    (when *debug*
      (princ (reverse out))
      (terpri))
    
    `(tagbody
	,@(nreverse out))))

(defun vm-compile (&key (start-pc 0) (end-pc (length *code*)))
  (eval `(lambda ()
	   (declare (optimize (debug 0) (safety 0) (speed 3)))
	   ,(compile-main :start-pc start-pc :end-pc end-pc))))

(defun vm-eval (&key (start-pc 0))
  (funcall (vm-compile :start-pc start-pc)))

(defun next-type-id ()
  (with-slots (type-count) *vm*
    (let ((id type-count))
      (incf type-count)
      (assert (< type-count *max-type-count*))
      id)))
