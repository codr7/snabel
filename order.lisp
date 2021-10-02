(in-package snabl)

(defmethod compare ((x number) (y number))
  (cond
    ((< x y) :lt)
    ((> x y) :gt)
    (t :eq)))

(defmethod compare ((x string) (y string))
  (cond
    ((string< x y) :lt)
    ((string> x y) :gt)
    (t :eq)))

(defmethod compare  ((x symbol) (y symbol))
  (if (eq (x y))
      :eq
      (compare (symbol-name x) (symbol-name y))))

