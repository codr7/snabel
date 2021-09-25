(defpackage clvm
  (:use cl)
  (:import-from utils char-digit reverse-vector sym)
  (:export *min-column* *min-line* *version* *vm*
	   abc-lib
	   clone column copy
	   data dump dump-stack
	   emit-form emit-forms emit-op eval
	   form
	   id-form int-type
	   line lit-form
	   new-branch-op new-goto-op new-pos new-push-op new-id-form new-lit-form new-val new-vm
	   op
	   parse-int parse-ws pc pos
	   source
	   vm-pop vm-push vm-push-new vm-type))

(in-package clvm)

(define-symbol-macro *version* 1)
(define-symbol-macro *max-reg-count* 64)

(defvar *vm*)
  
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
   (val-clone :initform #'identity
	      :reader val-clone)
   (val-dump :initform (lambda (v out)
			 (print-object v out))
	     :reader val-dump)
   (val-is-true? :initform (lambda (v)
			     (declare (ignore v))
			     t)
		 :reader val-is-true?)))
