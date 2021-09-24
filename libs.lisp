(in-package clvm)

(defclass bool-type (vm-type)
  ((name :initform :|Bool|)
   (is-true? :initform (lambda (v) v))))

(defclass int-type (vm-type)
  ((name :initform :|Int|)
   (is-true? :initform (lambda (v) (not (zerop v))))))

(defclass reg-type (vm-type)
  ((name :initform :|Reg|)))

(defclass abc-lib (lib)
  ((name :initform :abc)
   (bool-type :initform (make-instance 'bool-type) :reader bool-type)
   (int-type :initform (make-instance 'int-type) :reader int-type)
   (reg-type :initform (make-instance 'reg-type) :reader reg-type)))

(defmethod initialize-instance :after ((lib abc-lib) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (setf (lib-find lib :true) (new-val (bool-type lib) t))
  (setf (lib-find lib :false) (new-val (bool-type lib) nil)))
