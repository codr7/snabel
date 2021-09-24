(in-package clvm)

;; id

(defstruct (id-form (:include form) (:conc-name id-))
  (name (error "Missing name") :type keyword))

(defun new-id-form (name &key (pos *default-pos*))
  (make-id-form :pos pos :name name))

(defmethod emit ((f id-form))
  (error "Not implemented"))

;; lit

(defstruct (lit-form (:include form) (:conc-name lit-))
  (val (error "Missing val") :type val))

(defun new-lit-form (type data &key (pos *default-pos*))
  (make-lit-form :pos pos :val (new-val type data)))

(defmethod emit ((f lit-form))
  (let ((v (lit-val f)))
    (emit (new-push-op (vm-type v) (data v) :form f))))
