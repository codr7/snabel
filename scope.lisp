(in-package snabl)

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(defclass scope ()
  ((parent-scope :initform (error "Missing parent-scope") :initarg :parent-scope :reader parent-scope)
   (bindings :initform (make-hash-table) :reader bindings)
   (reg-count :initform 0 :reader reg-count)))

(defmethod initialize-instance :after ((scope scope) &rest args &key &allow-other-keys)
  (with-slots (parent-scope reg-count) scope
    (when parent-scope
      (setf reg-count (reg-count parent-scope)))))

(defmethod begin-scope (&key (proc *proc*))
  (with-slots (scope) proc
    (setf scope (make-instance 'scope :parent-scope scope))))

(defmacro with-scope ((scope) &body body)
  (let ((prev (gensym)))
    `(with-slots (scope) *proc*
       (let ((,prev scope))
	 (setf scope ,scope)
	 
	 (unwind-protect
	      (progn ,@body)
	   (setf scope ,prev))))))

(defmethod end-scope (&key (proc *proc*))
  (with-slots (scope) proc
    (setf scope (parent-scope scope))))

(defun scope-find (key &key (scope *scope*))
  (with-slots (bindings parent-scope) scope
    (let ((v (gethash key bindings)))
      (or v (and parent-scope (scope-find key :scope parent-scope))))))

(defun (setf scope-find) (val key)
  (with-slots (bindings) *scope*
    (when (gethash key bindings)
      (error "Dup binding: ~a" key))
    (setf (gethash key bindings) val)))

(defun scope-bind (key val)
  (setf (scope-find key) val))

(defun scope-bind-reg (key)
  (with-slots (reg-count) *scope*
    (let ((reg reg-count))
      (assert (< reg *max-reg-count*))
      (incf reg-count)
      (setf (scope-find key) (new-val (reg-type *abc-lib*) reg))
      reg)))
