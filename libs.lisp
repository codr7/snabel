(in-package clvm)

(defstruct (abc-lib (:include lib) (:conc-name))
  (int-type (make-vm-type :name :int)))
