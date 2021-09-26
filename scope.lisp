(in-package snabl)

(defclass scope ()
  ((parent-scope :initform nil :initarg :parent-scope :reader parent-scope)
   (bindings :initform (make-hash-table) :reader bindings)
   (reg-count :initform 0 :reader reg-count)))

(defmethod initialize-instance :after ((scope scope) &rest args &key &allow-other-keys)
  (with-slots (parent-scope reg-count) scope
    (when parent-scope
      (setf reg-count (reg-count parent-scope)))))

(defmethod begin-scope (&key (vm *vm*))
  (with-slots (scope) vm
    (setf scope (make-instance 'scope :parent-scope scope))))

(defmethod end-scope ()
  (with-slots (scope) *vm*
    (setf scope (parent-scope scope))))

(defun scope-find (key)
  (with-slots (bindings) *scope*
    (gethash key bindings)))

(defun (setf scope-find) (val key)
  (with-slots (bindings) *scope*
    (setf (gethash key bindings) val)))

(defun scope-bind (key val)
  (setf (scope-find key) val))

(defun scope-bind-reg (key)
  (with-slots (reg-count) *scope*
    (let ((reg (reg-count)))
      (assert (< reg *max-reg-count*))
      (incf reg-count)
      (setf (scope-find key) (new-val (reg-type *abc-lib*) reg))
      reg)))
