(in-package clvm)

(defstruct (goto-op (:include op))
  (label (error "Missing label") :type keyword))

(defun goto-op (label &key (form *default-form*))
  (make-goto-op :form form :label label))

(defmethod emit-lisp ((op goto-op))
  `(go ,(goto-op-label op)))

(defstruct (label-op (:include op))
  (name (error "Missing name") :type keyword))

(defun label-op (name &key (form *default-form*))
  (make-label-op :form form :name name))

(defmethod emit-lisp ((op label-op))
  (label-op-name op))

(defstruct (push-op (:include op))
  (val (error "Missing val") :type val))

(defun push-op (vm-type data &key (form *default-form*))
  (make-push-op :form form :val (val vm-type data)))

(defmethod emit-lisp ((op push-op))
  `(vm-push ,(vm-type (push-op-val op)) ,(data (push-op-val op))))
