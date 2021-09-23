(in-package clvm)

(defclass bool-type (vm-type)
  ((name :initform :bool)
   (is-true? :initform (lambda (v) v))))


(defclass int-type (vm-type)
  ((name :initform :int)
   (is-true? :initform (lambda (v) (not (zerop v))))))

(defclass abc-lib (lib)
  ((name :initform :abc)
   (bool-type :initform (make-instance 'bool-type) :reader bool-type)
   (int-type :initform (make-instance 'int-type) :reader int-type)))

(defmethod initialize-instance :after ((lib abc-lib) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (setf (lib-find lib :true) (val (bool-type lib) t))
  (setf (lib-find lib :false) (val (bool-type lib) nil)))
