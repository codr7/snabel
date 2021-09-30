(defpackage snabl
  (:use cl)
  (:import-from utils all? char-digit dohash dovector kw reverse-vector sym ws?)
  (:export *min-column* *min-line* *version* *vm*
	   abc-lib
	   clone column copy
	   data dump dump-stack
	   emit-forms emit-op eval
	   form form-emit
	   id-form int-type
	   line lit-form
	   new-branch-op new-goto-op new-pos new-push-op new-id-form new-lit-form new-val new-vm
	   op
	   parse-int parse-ws pc pos
	   source
	   vm-pop vm-push vm-push-new vm-type))

(in-package snabl)

(define-symbol-macro *version* 5)
(define-symbol-macro *max-reg-count* 64)
(define-symbol-macro *max-type-count* 64)

(define-symbol-macro *abc-lib* (with-slots (abc-lib) *vm*
				 (unless abc-lib
				   (setf abc-lib (make-instance 'abc-lib))
				   (init abc-lib))
				 abc-lib))

(define-symbol-macro *code* (with-slots (code) *vm* code))
(define-symbol-macro *pc* (with-slots (code) *vm* (length code)))
(define-symbol-macro *proc* (with-slots (proc-cache) *vm* proc-cache))
(define-symbol-macro *regs* (with-slots (regs) *proc* regs))
(define-symbol-macro *scope* (with-slots (scope) *proc* scope))
(define-symbol-macro *stack* (with-slots (stack) *proc* stack))

(defvar *vm*)
