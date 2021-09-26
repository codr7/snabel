(in-package snabl)

(defstruct op
  (form *default-form* :type form))

;; branch

(defstruct (branch-op (:include op) (:conc-name branch-))
  (false-label (error "Missing false-label") :type symbol))

(defun new-branch-op (false-label &key (form *default-form*))
  (make-branch-op :form form :false-label false-label))

(defmethod emit-lisp ((op branch-op))
  `(unless (is-true? (vm-pop))
     (go ,(branch-false-label op))))

;; copy

(defstruct (copy-op (:include op) (:conc-name copy-)))

(defun new-copy-op (&key (form *default-form*))
  (make-copy-op :form form))

(defmethod emit-lisp ((op copy-op))
  `(let ((v (vm-peek)))
     (unless v
       (error "Stack is empty"))
     (vm-push v)))

;; drop

(defstruct (drop-op (:include op) (:conc-name drop-))
  (count (error "Missing count") :type integer))

(defun new-drop-op (count &key (form *default-form*))
  (make-drop-op :form form :count count))

(defmethod emit-lisp ((op drop-op))
  `(unless (drop ,(drop-count op))
     (error "Stack is empty")))

;; goto

(defstruct (goto-op (:include op) (:conc-name goto-))
  (label (error "Missing label") :type symbol))

(defun new-goto-op (label &key (form *default-form*))
  (make-goto-op :form form :label label))

(defmethod emit-lisp ((op goto-op))
  `(go ,(goto-label op)))

;; label

(defstruct (label-op (:include op) (:conc-name label-))
  (name (error "Missing name") :type symbol))

(defun new-label-op (name &key (form *default-form*))
  (make-label-op :form form :name name))

(defmethod emit-lisp ((op label-op))
  (label-name op))

;; lisp

(defstruct (lisp-op (:include op) (:conc-name lisp-))
  (expr (error "Missing expr") :type function))

(defun new-lisp-op (expr &key (form *default-form*))
  (make-lisp-op :form form :expr expr))

(defmethod emit-lisp ((op lisp-op))
  `(funcall ,(lisp-expr op)))

;; load

(defstruct (load-op (:include op) (:conc-name load-))
  (reg (error "Missing reg") :type integer))

(defun new-load-op (reg &key (form *default-form*))
  (make-load-op :form form :reg reg))

(defmethod emit-lisp ((op load-op))
  `(vm-push (aref (regs *vm*) ,(load-reg op))))

;; push

(defstruct (push-op (:include op) (:conc-name push-))
  (val (error "Missing val") :type val))

(defun new-push-op (val &key (form *default-form*))
  (make-push-op :form form :val val))

(defmethod emit-lisp ((op push-op))
  `(vm-push (clone ,(push-val op))))

;; store

(defstruct (store-op (:include op) (:conc-name store-))
  (reg (error "Missing reg") :type integer)
  (val nil :type t))

(defun new-store-op (reg &key (form *default-form*) val)
  (make-store-op :form form :reg reg :val val))

(defmethod emit-lisp ((op store-op))
  `(setf (aref (regs *vm*) ,(store-reg op))
	 (or ,(store-val op) (vm-pop))))
