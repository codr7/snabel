(in-package snabl)

(defclass lib ()
  ((name :initform (error "Missing name") :reader name)
   (bindings :initform (make-hash-table) :reader bindings)))

(defun lib-find (lib key)
  (gethash key (bindings lib)))

(defun (setf lib-find) (val lib key)
  (setf (gethash key (bindings lib)) val))

(defun lib-import (lib &rest keys)
  (if keys
      (dolist (k keys)
	(let ((v (lib-find lib k)))
	  (unless v
	    (error "Unknown id: ~a" k))
	  (setf (scope-find k) v)))
      (dohash (k v (bindings lib))
	(setf (scope-find k) v))))

(defmacro bind-type (lib type)
  `(setf (lib-find ,lib (name (,type ,lib))) (new-val (meta-type ,lib) (,type ,lib))))

;; abc

(defclass bool-type (vm-type)
  ((name :initform :|Bool|)
   (val-dump :initform (lambda (v out) (write-string (if v "true" "false") out)))
   (val-is-true? :initform (lambda (v) v))))

(defclass int-type (vm-type)
  ((name :initform :|Int|)
   (val-is-true? :initform (lambda (v) (not (zerop v))))))

(defclass meta-type (vm-type)
  ((name :initform :|Meta|)))

(defclass reg-type (vm-type)
  ((name :initform :|Reg|)))

(defclass abc-lib (lib)
  ((name :initform :abc)
   (bool-type :initform (make-instance 'bool-type) :reader bool-type)
   (int-type :initform (make-instance 'int-type) :reader int-type)
   (meta-type :initform (make-instance 'meta-type) :reader meta-type)
   (reg-type :initform (make-instance 'reg-type) :reader reg-type)))

(defmethod initialize-instance :after ((lib abc-lib) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (bind-type lib bool-type)
  (bind-type lib int-type)
  (bind-type lib meta-type)
  (bind-type lib reg-type)
  
  (setf (lib-find lib :true) (new-val (bool-type lib) t))
  (setf (lib-find lib :false) (new-val (bool-type lib) nil)))
