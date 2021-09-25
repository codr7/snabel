(in-package lila)

(define-symbol-macro *min-line* 1)
(define-symbol-macro *min-column* 0)

(defstruct (pos (:conc-name))
  (source "n/a" :type string)
  (line *min-line* :type integer)
  (column *min-column* :type integer))

(defun new-pos (source &optional (line *min-line*) (column *min-column*))
  (make-pos :source source :line line :column column))

(defvar *default-pos* (new-pos "n/a"))
