(in-package clvm)

(defstruct (abc-lib (:include lib) (:conc-name))
  (integer-type (make-vm-type :name :integer)))
