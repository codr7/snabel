(in-package snabl)

(defstruct form
  (pos *default-pos* :type pos))

(defvar *default-form* (make-form :pos *default-pos*))

;; id

(defstruct (id-form (:include form) (:conc-name id-))
  (name (error "Missing name") :type keyword))

(defun new-id-form (name &key (pos *default-pos*))
  (make-id-form :pos pos :name name))

(defmethod emit-form ((f id-form) in)
  (let* ((k (id-name f))
	 (ks (symbol-name k)))
    (cond
      ((all? ks (lambda (c) (char= c #\d)))
       (emit-op (new-drop-op (length ks) :form f)))
      ((eq k :!)
       (let ((f (pop in))
	     (end-label (gensym)))
	 (emit-op (new-goto-op end-label :form f))
	 (let* ((start-pc (pc))
		(stack (stack *vm*))
		(prev-len (length stack)))
	   (emit-form f nil)
	   (vm-eval :pc start-pc)
	   (emit-op (new-label-op end-label :form f))
	   (dovector (v (subseq stack prev-len))
	     (push (new-lit-form v :pos (form-pos f)) in))
	   (drop (- (length stack) prev-len)))))
      (t (let ((v (scope-find k)))
	   (cond
	     ((null v)
	      (error "Unknown id: ~a" k))
	     ((eq (vm-type v) (reg-type (abc-lib)))
	      (emit-op (new-load-op (data v) :form f)))
	     (t (emit-op (new-push-op (copy v) :form f))))))))
  in)

;; lisp

(defstruct (lisp-form (:include form) (:conc-name lisp-))
  (expr (error "Missing expr") :type function))

(defun new-lisp-form (expr &key (pos *default-pos*))
  (make-lisp-form :pos pos :expr expr))

(defmethod emit-form ((f lisp-form) in)
  (emit-op (new-lisp-op (lisp-expr f) :form f))
  in)

;; lit

(defstruct (lit-form (:include form) (:conc-name lit-))
  (val (error "Missing val") :type val))

(defun new-lit-form (val &key (pos *default-pos*))
  (make-lit-form :pos pos :val val))

(defmethod emit-form ((f lit-form) in)
  (let ((v (lit-val f)))
    (emit-op (new-push-op (clone v) :form f)))
  in)
