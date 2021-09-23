(defpackage clvm
  (:use cl)
  (:import-from utils sym)
  (:export *version* form op pos val-type val))

(in-package clvm)

(define-symbol-macro *version* 1)

(defvar *vm*)

(defstruct pos
  (source "n/a" :type string)
  (line -1 :type integer)
  (column -1 :type integer))

(defvar *default-pos* (make-pos))

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
