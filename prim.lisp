(in-package snabl)

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(defclass prim ()
  ((name :initarg :name :initform (error "Missing name") :reader name)
   (arg-count :initarg :arg-count :initform (error "Missing arg count") :reader arg-count)
   (body :initarg :body :initform (error "Missing body") :reader body)))

(defun new-prim (name arg-count body)
  (make-instance 'prim :name name :arg-count arg-count :body body))

(defun prim-call (self form in)
  (when (< (length in) (arg-count self))
    (error "Not enough arguments: ~a ~a" self in))
  (funcall (body self) self form in))

(defmethod print-object ((self prim) out)
  (format out "Prim(~a ~a)" (symbol-name (name self)) (arg-count self)))
