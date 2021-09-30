(in-package snabl)

(defclass func ()
  ((name :initarg :name :initform (error "Missing name") :reader name)
   (args :initarg :args :initform (error "Missing args") :reader args)
   (rets :initarg :rets :initform (error "Missing rets") :reader rets)
   (body :initarg :body :initform (error "Missing body") :reader body)))

(defun new-func (name args rets body)
  (make-instance 'func :name name :args args :rets rets :body body))

(defmethod applicable? ((self func))
  (with-slots (args) self
    (when (< (length *stack*) (length args))
      (return-from applicable?))

    (dotimes (i (length args))
      (let ((parent (rest (aref args (- (length args) i 1))))
	    (child (vm-type (aref *stack* (- (length *stack*) i 1)))))
	(unless (isa child parent)
	  (return-from applicable?)))))

  t)

(defmethod call ((self func) pos ret-label)
  (funcall (body self) self pos ret-label))

(defmethod print-object ((self func) out)
  (format out "Func(~a ~a ~a)" (symbol-name (name self)) (args self) (rets self)))
