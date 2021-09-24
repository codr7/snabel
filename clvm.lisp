(defpackage clvm
  (:use cl)
  (:import-from utils sym)
  (:export *min-column* *min-line* *version*
	   abc-lib column data
	   emit eval
	   form
	   id-form int-type
	   line lit-form
	   op
	   parse-int parse-ws pc pos
	   source
	   val vm-type))

(in-package clvm)

(define-symbol-macro *version* 1)

(defvar *vm*)

(defvar *min-line* 1)
(defvar *min-column* 0)

(defstruct (pos (:conc-name))
  (source "n/a" :type string)
  (line *min-line* :type integer)
  (column *min-column* :type integer))

(defvar *default-pos* (make-pos))

(defun pos (source &optional (line *min-line*) (column *min-column*))
  (make-pos :source source :line line :column column))
  
(defstruct form
  (pos *default-pos* :type pos))

(defvar *default-form* (make-form :pos *default-pos*))

(defstruct op
  (form *default-form* :type form))

(defun op (name)
  (fdefinition (sym name '-op)))

(defclass lib ()
  ((name :initform (error "Missing name") :reader name)
   (bindings :initform (make-hash-table) :reader bindings)))

(defun lib-find (lib key)
  (gethash key (bindings lib)))

(defun (setf lib-find) (val lib key)
  (setf (gethash key (bindings lib)) val))

(defclass vm-type ()
  ((name :initform (error "Missing name") :reader name)
   (is-true? :initform (lambda (v)
			 (declare (ignore v))
			 t)
	     :reader is-true?)))

(defstruct (val (:conc-name))
  (vm-type (error "Missing type") :type vm-type)
  (data (error "Missing data") :type t))

(defun val (vm-type data)
  (make-val :vm-type vm-type :data data))

(defmethod is-true? ((val val))
  (funcall (is-true? (vm-type val)) (data val)))
