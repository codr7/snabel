(in-package clvm)

(defstruct (push-op (:include op))
  (val (:error "Missing val") :type val))

(defmethod emit-lisp ((op push-op))
  `(vm-push ,(val-type (push-op-val op)) ,(val-data (push-op-val op))))
