(in-package clvm)

(defstruct (literal (:include form))
  (val (error "Missing val") :type val))

(defmethod emit ((f literal))
  (emit (make-push-op :form f :val (literal-val f))))
