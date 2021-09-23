(in-package clvm)

(defstruct (id (:include form))
  (name (error "Missing name") :type keyword))

(defun id (pos name)
  (make-id :pos pos :name name))

(defmethod emit ((f id))
  (error "Not implemented"))

(defstruct (lit (:include form))
  (val (error "Missing val") :type val))

(defun lit (pos type data)
  (make-lit :pos pos :val (val type data)))

(defmethod emit ((f lit))
  (emit (make-push-op :form f :val (lit-val f))))
