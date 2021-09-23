(in-package clvm)

;; id

(defstruct (id-form (:include form) (:conc-name id-))
  (name (error "Missing name") :type keyword))

(defun id-form (name &key (pos *default-pos*))
  (make-id-form :pos pos :name name))

(defmethod emit ((f id-form))
  (error "Not implemented"))

;; lit

(defstruct (lit-form (:include form) (:conc-name lit-))
  (val (error "Missing val") :type val))

(defun lit-form (type data &key (pos *default-pos*))
  (make-lit-form :pos pos :val (val type data)))

(defmethod emit ((f lit-form))
  (emit (make-push-op :form f :val (lit-val f))))
