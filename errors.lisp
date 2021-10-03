(in-package snabl)

(define-condition e (error)
  ((pos :initarg :pos :reader pos)
   (msg :initarg :msg :reader msg))
  (:report (lambda (self out)
	     (format out "Error ~a:~%~a" (pos self) (msg self)))))

(define-condition e-parse (e)
  ())

(defun e-parse (pos spec &rest args)
  (error 'e-parse :pos pos :msg (apply #'format nil spec args)))

(define-condition e-emit (e)
  ())

(defun e-emit (pos spec &rest args)
  (error 'e-emit :pos pos :msg (apply #'format nil spec args)))

(define-condition e-eval (e)
  ())

(defun e-eval (pos spec &rest args)
  (error 'e-eval :pos pos :msg (apply #'format nil spec args)))
