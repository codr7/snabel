(defpackage snabl
  (:use cl)
  (:import-from utils all? char-digit dohash kw reverse-vector sym ws?)
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

(in-package snabl)

(define-symbol-macro *version* 1)
(define-symbol-macro *max-reg-count* 64)

(defvar *vm*)
  
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
