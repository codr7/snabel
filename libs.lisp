(in-package clvm)

(defclass abc-lib (lib)
  ((name :initform :abc)
   (bool-type :initform (make-vm-type :name :bool) :reader bool-type)
   (int-type :initform (make-vm-type :name :int) :reader int-type)))

(defmethod initialize-instance :after ((lib abc-lib) &rest args &key &allow-other-keys)
  (setf (lib-find lib :true) (val (bool-type lib) t))
  (setf (lib-find lib :false) (val (bool-type lib) nil)))
