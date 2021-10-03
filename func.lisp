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
		 (setf body (lambda (self pos)
			      (let ((f (new-frame pos self)))
				(capture f)
				(push-frame f)
			      (unwind-protect
				   (funcall lisp-func))
				(pop-frame)
				(restore f)))))
	       
	       (emit-op (new-label-op end-label :form body-form))))
	(end-scope))))
  in)

(defmethod applicable? ((self func))
  (with-slots (args) self
    (when (< (length *stack*) (length args))
      (return-from applicable?))

    (dotimes (i (length args))
      (let ((parent (arg-vm-type (aref args (- (length args) i 1))))
	    (child (vm-type (aref *stack* (- (length *stack*) i 1)))))
	(unless (isa child parent)
	  (format t "applicable? ~a~%" *regs*)
	  (return-from applicable?)))))

  t)

(defmethod call ((self func) pos)
  (funcall (body self) self pos))

(defmethod print-object ((self func) out)
  (format out "Func(~a ~a ~a)" (symbol-name (name self)) (args self) (rets self)))
