(in-package snabl)

(defstruct (form)
  (pos *default-pos* :type pos))

(defvar *default-form* (make-form :pos *default-pos*))

(defmethod form-val ((f form)))

;; cte

(defstruct (cte-form (:include form) (:conc-name cte-))
  (expr (error "Missing expr") :type form))

(defun new-cte-form (expr &key (pos *default-pos*))
  (make-cte-form :pos pos :expr expr))

(defmethod form-emit ((f cte-form) in)
  (let ((end-label (gensym)))
    (emit-op (new-goto-op end-label :form f))
    (let* ((start-pc *pc*)
	   (prev-len (length *stack*)))
      (setf in (form-emit (cte-expr f) in))
      (vm-eval :pc start-pc)
      (emit-op (new-label-op end-label :form f))
      (dovector (v (subseq *stack* prev-len))
	(push (new-lit-form v :pos (form-pos f)) in))
      (drop (- (length *stack*) prev-len))))
  in)

;; dot

(defstruct (dot-form (:include form) (:conc-name dot-))
  (expr (error "Missing expr") :type form))

(defun new-dot-form (expr &key (pos *default-pos*))
  (make-dot-form :pos pos :expr expr))

(defmethod form-emit ((f dot-form) in)
  (let ((rf (pop in)))
    (unless rf
      (e-emit (form-pos f) "Missing dot form"))
    (push (dot-expr f) in)
    (push rf in))
  in)

;; group

(defstruct (group-form (:include form) (:conc-name group-))
  (body (error "Missing body") :type list))

(defun new-group-form (body &key (pos *default-pos*))
  (make-group-form :pos pos :body body))

(defmethod form-emit ((f group-form) in)
  (append (group-body f) in))

;; id

(defstruct (id-form (:include form) (:conc-name id-))
  (name (error "Missing name") :type keyword))

(defun new-id-form (name &key (pos *default-pos*))
  (make-id-form :pos pos :name name))

(defmethod form-emit ((f id-form) in)
  (let* ((k (id-name f))
	 (ks (symbol-name k)))
    (cond
      ((all? ks (lambda (c) (char= c #\d)))
       (emit-op (new-drop-op (length ks) :form f)))
      (t (let ((v (scope-find k)))
	   (cond
	     ((null v)
	      (e-emit (form-pos f) "Unknown id: ~a" k))
	     ((eq (vm-type v) (prim-type *abc-lib*))
	      (return-from form-emit (prim-call (data v) f in)))
	     ((eq (vm-type v) (reg-type *abc-lib*))
	      (emit-op (new-load-op (data v) :form f)))
	     (t (emit-op (new-push-op (copy v) :form f))))))))
  in)

(defmethod form-val ((f lit-form))
  (let ((v (scope-find (id-name f))))
    (when (and v (not (eq (vm-type v) (reg-type *abc-lib*))))
      v)))

;; lisp

(defstruct (lisp-form (:include form) (:conc-name lisp-))
  (expr (error "Missing expr") :type function))

(defun new-lisp-form (expr &key (pos *default-pos*))
  (make-lisp-form :pos pos :expr expr))

(defmethod form-emit ((f lisp-form) in)
  (emit-op (new-lisp-op (lisp-expr f) :form f))
  in)

;; lit

(defstruct (lit-form (:include form) (:conc-name lit-))
  (val (error "Missing val") :type val))

(defun new-lit-form (val &key (pos *default-pos*))
  (make-lit-form :pos pos :val val))

(defmethod form-emit ((f lit-form) in)
  (let ((v (lit-val f)))
    (emit-op (new-push-op (clone v) :form f)))
  in)

(defmethod form-val ((f lit-form))
  (lit-val f))

;; nop

(defstruct (nop-form (:include form) (:conc-name nop-)))

(defun new-nop-form (&key (pos *default-pos*))
  (make-nop-form :pos pos))

(defmethod form-emit ((f nop-form) in)
  in)

;; scope

(defstruct (scope-form (:include form) (:conc-name scope-))
  (body (error "Missing body") :type list))

(defun new-scope-form (body &key (pos *default-pos*))
  (make-scope-form :pos pos :body body))

(defmethod form-emit ((f scope-form) in)
  (begin-scope)
  (emit-forms (scope-body f))
  (end-scope)
  in)
