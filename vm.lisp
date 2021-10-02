(in-package snabl)

(defclass vm (proc)
  ((type-count :initform 0)
   (abc-lib :initform nil)
   (math-lib :initform nil)
   (code :initform (make-array 0 :element-type 'op :fill-pointer 0) :reader code)
   (main-scope :reader main-scope)
   (procs :initform (make-array 0 :element-type 'proc :fill-pointer 0) :reader procs)
   (proc-cache :initform nil :reader proc-cache)
   (frames :initform (make-array 0 :element-type 'frame :fill-pointer 0) :reader frames)
   (frame-cache :initform nil :reader frame-cache)))

(defun new-vm ()
  (make-instance 'vm))

(defmethod initialize-instance :after ((self vm) &rest args &key &allow-other-keys)
  (with-slots (main-scope proc-cache) self
    (push-proc self :vm self)
    (setf main-scope (begin-scope :proc proc-cache))))

(defun emit-op (op)
  (vector-push-extend op (code *vm*)))

(defun emit-forms (forms)
  (labels ((rec (in)
	     (when in
	       (rec (form-emit (first in) (rest in))))))
    (rec forms)))

(defun compile-main (&key (pc 0))  
  (let (out)
    (dotimes (i (- (length (code *vm*)) pc))
      (let ((op (aref (code *vm*) (+ pc i))))
	(push (emit-lisp op) out)))
    (when *debug*
      (format t "(TAGBODY~%  ~a)~%" (reverse out)))
    
    `(tagbody
	,@(nreverse out))))

(defun vm-compile (&key (pc 0))
  (eval `(lambda () ,(compile-main :pc pc))))

(defun vm-eval (&key (pc 0))
  (funcall (vm-compile :pc pc)))

(defun next-type-id ()
  (with-slots (type-count) *vm*
    (let ((id type-count))
      (incf type-count)
      (assert (< type-count *max-type-count*))
      id)))
