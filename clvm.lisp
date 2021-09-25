(defpackage clvm
  (:use cl)
  (:import-from utils char-digit reverse-vector sym)
  (:export *min-column* *min-line* *version* *vm*
	   abc-lib column
	   data dump dump-stack
	   emit-form emit-forms emit-op eval
	   form
	   id-form int-type
	   line lit-form
	   new-branch-op new-goto-op new-pos new-push-op new-id-form new-lit-form new-val new-vm
	   op
	   parse-int parse-ws pc pos
	   source
	   vm-type))

(in-package clvm)

(define-symbol-macro *version* 1)
(define-symbol-macro *max-reg-count* 64)

(define-symbol-macro *min-line* 1)
(define-symbol-macro *min-column* 0)

(defvar *vm*)

(defstruct (pos (:conc-name))
  (source "n/a" :type string)
  (line *min-line* :type integer)
  (column *min-column* :type integer))

(defvar *default-pos* (make-pos))

(defun new-pos (source &optional (line *min-line*) (column *min-column*))
  (make-pos :source source :line line :column column))
  
(defstruct form
  (pos *default-pos* :type pos))

(defvar *default-form* (make-form :pos *default-pos*))

(defstruct op
  (form *default-form* :type form))

(defclass lib ()
  ((name :initform (error "Missing name") :reader name)
   (bindings :initform (make-hash-table) :reader bindings)))

(defun lib-find (lib key)
  (gethash key (bindings lib)))

(defun (setf lib-find) (val lib key)
  (setf (gethash key (bindings lib)) val))

(defclass vm-type ()
  ((name :initform (error "Missing name") :reader name)
   (val-dump :initform (lambda (v out)
			 (print-object v out))
	     :reader val-dump)
   (val-is-true? :initform (lambda (v)
			     (declare (ignore v))
			     t)
		 :reader val-is-true?)))
