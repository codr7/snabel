(in-package snabl)

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(defstruct arg
  (name nil :type symbol)
  (vm-type (error "Missing type") :type vm-type)
  (reg nil))

(defun new-arg (name vm-type)
  (make-arg :name name :vm-type vm-type))

(defclass func ()
  ((name :initarg :name :initform (error "Missing name") :reader name)
   (args :initarg :args :initform (error "Missing args") :reader args)
   (rets :initarg :rets :initform (error "Missing rets") :reader rets)
   (body :initarg :body :initform (error "Missing body") :reader body)
   (body-scope :initform nil :reader body-scope)
   (start-label :initform nil :reader start-label)
   (max-reg :initform 0 :reader max-reg)
   (min-reg :initform 0 :reader min-reg)))

(defun new-func (name args rets body)
  (make-instance 'func :name name :args args :rets rets :body body))

(defun func-emit (self body-form in)
  (with-slots (args body body-scope max-reg min-reg start-label) self
    (let ((lisp-name (gensym))
	  (end-label (gensym)))
      (emit-op (new-goto-op end-label :form body-form))
      
      (unwind-protect
	   (progn
	     (setf body-scope (begin-scope))
	     (setf min-reg (reg-count body-scope))
	     
	     (dovector (a args)
	       (when (arg-name a)
		 (setf max-reg (max max-reg (setf (arg-reg a)
						  (scope-bind-reg (arg-name a)))))))

	     (let ((offset 0)
		   (start-pc *pc*))
	       (setf start-label (gensym))
	       (emit-op (new-label-op start-label :form body-form))
	       
	       (dotimes (i (length args))
		 (let ((a (aref args (- (length args) i 1))))
		   (if (arg-name a)
		       (emit-op (new-store-op (arg-reg a) :offset offset :form body-form))
		       (incf offset))))
	       
	       (setf in (form-emit body-form in))
	       
	       (let ((lisp-func (vm-compile :start-pc start-pc)))
		 (setf body (lambda (self pos &key drop-rets?)
			      (let ((f (new-frame pos self)))
				(capture f)
				(push-frame f)
				(unwind-protect
				     (funcall lisp-func))
				(pop-frame)
				(restore f :drop-rets? drop-rets?)))))
	       
	       (emit-op (new-label-op end-label :form body-form))))
	(end-scope))))
  in)

(defun func-applicable? (self)
  (declare (type func self))
  
  (with-slots (args) self
    (when (< (length *stack*) (length args))
      (return-from func-applicable?))

    (let ((arg-offset (- (length args) 1))
	  (stack-offset (- (length *stack*) 1)))
      (dotimes (i (length args))
	(declare (type integer i))
	
	(let ((parent (arg-vm-type (aref args (- arg-offset i))))
	      (child (vm-type (aref *stack* (- stack-offset i)))))
	  (unless (isa child parent)
	    (return-from func-applicable?))))))

  t)

(defmethod call ((self func) pos &key drop-rets? unsafe?)
  (unless (or *unsafe?* (func-applicable? self))
    (dump-stack :out *standard-error*)
    (e-eval pos "Not applicable: ~a" self))

  (when unsafe?
    (incf (slot-value *vm* 'unsafe-depth)))
  
  (unwind-protect
       (funcall (body self) self pos :drop-rets? drop-rets?)
    (when unsafe?
      (decf (slot-value *vm* 'unsafe-depth)))))
  

(defmethod print-object ((self func) out)
  (format out "Func(~a ~a ~a)" (symbol-name (name self)) (args self) (rets self)))
