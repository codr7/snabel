(in-package clvm)

;; branch

(defstruct (branch-op (:include op) (:conc-name branch-))
  (false-label (error "Missing false-label") :type keyword))

(defun new-branch-op (false-label &key (form *default-form*))
  (make-branch-op :form form :false-label false-label))

(defmethod emit-lisp ((op branch-op))
  `(unless (is-true? (vm-pop))
     (go ,(branch-false-label op))))

;; goto

(defstruct (goto-op (:include op) (:conc-name goto-))
  (label (error "Missing label") :type keyword))

(defun new-goto-op (label &key (form *default-form*))
  (make-goto-op :form form :label label))

(defmethod emit-lisp ((op goto-op))
  `(go ,(goto-label op)))

;; label

(defstruct (label-op (:include op) (:conc-name label-))
  (name (error "Missing name") :type keyword))

(defun new-label-op (name &key (form *default-form*))
  (make-label-op :form form :name name))

(defmethod emit-lisp ((op label-op))
  (label-name op))

;; push

(defstruct (push-op (:include op) (:conc-name push-))
  (val (error "Missing val") :type val))

(defun new-push-op (vm-type data &key (form *default-form*))
  (make-push-op :form form :val (new-val vm-type data)))

(defmethod emit-lisp ((op push-op))
  `(vm-push ,(vm-type (push-val op)) ,(data (push-val op))))
