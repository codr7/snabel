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

;; load

(defstruct (load-op (:include op) (:conc-name load-))
  (reg (error "Missing reg") :type reg))

(defun new-load-op (reg &key (form *default-form*))
  (make-load-op :form form :reg reg))

(defmethod emit-lisp ((op load-op))
  `(aref (regs *vm*) ,(load-reg op)))

;; push

(defstruct (push-op (:include op) (:conc-name push-))
  (val (error "Missing val") :type val))

(defun new-push-op (val &key (form *default-form*))
  (make-push-op :form form :val val))

(defmethod emit-lisp ((op push-op))
  `(vm-push (clone ,(push-val op))))

;; store

(defstruct (store-op (:include op) (:conc-name store-))
  (reg (error "Missing reg") :type reg)
  (val nil :type val))

(defun new-store-op (reg &key (form *default-form*) val)
  (make-store-op :form form :reg reg :val val))

(defmethod emit-lisp ((op store-op))
  `(setf (aref (regs *vm*) ,(store-reg op))
	 (or ,(store-val op) (vm-pop))))
