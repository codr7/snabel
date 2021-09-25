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
  (let ((v (scope-find (id-name f))))
    (cond
      ((null v)
       (error "Unknown id: ~a" (id-name f)))
      ((eq (vm-type v) (reg-type (abc-lib *vm*)))
       (emit-op (new-load-op (data v))))
      (t (emit-op (new-push-op (copy v) :form f)))))
  in)

;; lit

(defstruct (lit-form (:include form) (:conc-name lit-))
  (val (error "Missing val") :type val))

(defun new-lit-form (type data &key (pos *default-pos*))
  (make-lit-form :pos pos :val (new-val type data)))

(defmethod emit-form ((f lit-form) in)
  (let ((v (lit-val f)))
    (emit-op (new-push-op (clone v) :form f)))
  in)
