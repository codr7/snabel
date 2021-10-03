(in-package snabl)

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(defstruct op
  (form *default-form* :type form))

;; bench

(defstruct (bench-op (:include op) (:conc-name bench-))
  (reps (error "Missing reps") :type integer)
  (start-pc (error "Missing start pc") :type integer)
  (end-pc nil)
  (end-label (error "Missing end label") :type symbol))

(defun new-bench-op (reps start-pc end-label &key (form *default-form*))
  (make-bench-op :form form :reps reps :start-pc start-pc :end-label end-label))

(defmethod emit-lisp ((op bench-op))
  (let ((imp (vm-compile :start-pc (bench-start-pc op) :end-pc (bench-end-pc op))))
    `(let ((start-time (get-internal-real-time)))
       (dotimes (i ,(bench-reps op))
	 (funcall ,imp))
     
       (vm-push (new-val ,(int-type *abc-lib*)
			 (round (/ (- (get-internal-real-time) start-time)
				   time-units-per-ms))))
       (go ,(bench-end-label op)))))
    
;; branch

(defstruct (branch-op (:include op) (:conc-name branch-))
  (false-label (error "Missing false-label") :type symbol))

(defun new-branch-op (false-label &key (form *default-form*))
  (make-branch-op :form form :false-label false-label))

(defmethod emit-lisp ((op branch-op))
  `(unless (true? (the val (vm-pop)))
     (go ,(branch-false-label op))))

;; call

(defstruct (call-op (:include op) (:conc-name call-))
  (target nil)
  (drop-rets? (error "Missing drop-rets?") :type boolean)
  (unsafe? (error "Missing unsafe?") :type boolean))

(defun new-call-op (target &key drop-rets? (form *default-form*) unsafe?)
  (cond
    ((eq target (int-add-func *math-lib*))
     (new-inc-op :form form))
    ((eq target (int-sub-func *math-lib*))
     (new-dec-op :form form))
    (t
     (make-call-op :form form :target target :drop-rets? drop-rets? :unsafe? unsafe?))))

(defmethod emit-lisp ((op call-op))
  (let ((pos (pos (call-form op))))
    `(let ((target (or ,(call-target op) (vm-pop))))
       (unless target
	 (e-eval ,pos "Missing call target"))
       (call target ,pos :drop-rets? ,(call-drop-rets? op) :unsafe? ,(call-unsafe? op)))))

;; copy

(defstruct (copy-op (:include op) (:conc-name copy-)))

(defun new-copy-op (&key (form *default-form*))
  (make-copy-op :form form))

(defmethod emit-lisp ((op copy-op))
  `(let ((v (vm-peek)))
     (unless v
       (e-emit ,(pos (op-form op)) "Stack is empty"))
     (vm-push (copy (the val v)))))

;; dec

(defstruct (dec-op (:include op) (:conc-name dec-)))

(defun new-dec-op (&key (form *default-form*))
  (make-dec-op :form form))

(defmethod emit-lisp ((op dec-op))
  `(let ((y (vm-pop)))
     (decf (data (vm-peek)) (data y))))

;; drop

(defstruct (drop-op (:include op) (:conc-name drop-))
  (count (error "Missing count") :type integer))

(defun new-drop-op (count &key (form *default-form*))
  (make-drop-op :form form :count count))

(defmethod emit-lisp ((op drop-op))
  `(unless (drop (the integer ,(drop-count op)))
     (e-emit ,(pos (op-form op)) "Stack is empty")))

;; goto

(defstruct (goto-op (:include op) (:conc-name goto-))
  (label (error "Missing label") :type symbol))

(defun new-goto-op (label &key (form *default-form*))
  (make-goto-op :form form :label label))

(defmethod emit-lisp ((op goto-op))
  `(go ,(goto-label op)))

;; inc

(defstruct (inc-op (:include op) (:conc-name inc-)))

(defun new-inc-op (&key (form *default-form*))
  (make-inc-op :form form))

(defmethod emit-lisp ((op inc-op))
  `(let ((y (vm-pop)))
     (incf (data (vm-peek)) (data y))))

;; is

(defstruct (is-op (:include op) (:conc-name is-))
  (x nil)
  (y nil))

(defun new-is-op (x y &key (form *default-form*))
  (make-is-op :form form :x x :y y))

(defmethod emit-lisp ((op is-op))
  `(let ((y (or ,(is-y op) (vm-pop)))
	 (x (or ,(is-x op) (vm-pop))))
     (vm-push (new-val (bool-type *abc-lib*) (is x y)))))

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
  `(vm-push (the val (clone ,(push-val op)))))

;; store

(defstruct (store-op (:include op) (:conc-name store-))
  (reg (error "Missing reg") :type integer)
  (offset 0)
  (val nil))

(defun new-store-op (reg &key (form *default-form*) (offset 0) val)
  (make-store-op :form form :reg reg :offset offset :val val))

(defmethod emit-lisp ((op store-op))
  `(setf (get-reg ,(store-reg op))
	 (the val (or ,(store-val op)
		      (vm-pop :offset ,(store-offset op))))))
