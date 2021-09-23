(in-package clvm)

(defstruct (goto-op (:include op))
  (label (:error "Missing label") :type keyword))

(defmethod emit-lisp ((op goto-op))
  `(go ,(goto-op-label op)))

(defstruct (label-op (:include op))
  (name (:error "Missing name") :type keyword))

(defmethod emit-lisp ((op label-op))
  (label-op-name op))

(defstruct (push-op (:include op))
  (val (:error "Missing val") :type val))

(defmethod emit-lisp ((op push-op))
  `(vm-push ,(val-type (push-op-val op)) ,(val-data (push-op-val op))))
