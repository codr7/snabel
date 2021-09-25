(in-package snabl)

(defclass lib ()
  ((name :initform (error "Missing name") :reader name)
   (bindings :initform (make-hash-table) :reader bindings)))

(defun lib-find (lib key)
  (gethash key (bindings lib)))

(defun (setf lib-find) (val lib key)
  (setf (gethash key (bindings lib)) val))

(defun lib-import (lib &rest keys)
  (dolist (k keys)
    (let ((v (lib-find lib k)))
      (unless v
	(error "Unknown id: ~a" k))
      (setf (scope-find k) v))))
      
    
