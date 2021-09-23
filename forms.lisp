(in-package clvm)

(defstruct (lit (:include form))
  (val (error "Missing val") :type val))

(defun lit (pos type data)
  (make-lit :pos pos :val (val type data)))

(defmethod emit ((f lit))
  (emit (make-push-op :form f :val (lit-val f))))
