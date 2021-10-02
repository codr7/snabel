(in-package snabl)

(defstruct op
  (form *default-form* :type form))

;; branch

(defstruct (branch-op (:include op) (:conc-name branch-))
  (false-label (error "Missing false-label") :type symbol))

(defun new-branch-op (false-label &key (form *default-form*))
  (make-branch-op :form form :false-label false-label))

(defmethod emit-lisp ((op branch-op))
  `(unless (true? (vm-pop))
     (go ,(branch-false-label op))))

;; call

(defstruct (call-op (:include op) (:conc-name call-))
  (target nil))

(defun new-call-op (target &key (form *default-form*))
  (make-call-op :form form :target target))

(defmethod emit-lisp ((op call-op))
  (let ((pos (form-pos (call-form op))))
    `(let ((target (or ,(call-target op) (vm-pop))))
       (unless target
	 (e-eval ,pos "Missing call target"))
       (unless (applicable? target)
	 (e-eval ,pos "Not applicable: ~a" target))
       (call target ,pos))))

;; copy

(defstruct (copy-op (:include op) (:conc-name copy-)))

(defun new-copy-op (&key (form *default-form*))
  (make-copy-op :form form))

(defmethod emit-lisp ((op copy-op))
  `(let ((v (vm-peek)))
     (unless v
       (e-emit ,(form-pos (op-form op)) "Stack is empty"))
     (vm-push (copy v))))

;; drop

(defstruct (drop-op (:include op) (:conc-name drop-))
  (count (error "Missing count") :type integer))

(defun new-drop-op (count &key (form *default-form*))
  (make-drop-op :form form :count count))

(defmethod emit-lisp ((op drop-op))
  `(unless (drop ,(drop-count op))
     (e-emit ,(form-pos (op-form op)) "Stack is empty")))

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
  (reg (error "Missing reg") :type integer)
  (vm-type nil))

(defun new-load-op (reg &key (form *default-form*))
  (make-load-op :form form :reg reg))

(defmethod emit-lisp ((op load-op))
  `(vm-push (clone (get-reg ,(load-reg op)))))

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
  (offset 0)
  (val nil))

(defun new-store-op (reg &key (form *default-form*) (offset 0) val)
  (make-store-op :form form :reg reg :offset offset :val val))

(defmethod emit-lisp ((op store-op))
  `(setf (get-reg ,(store-reg op))
	 (or ,(store-val op)
	     (vm-pop :offset ,(store-offset op)))))
