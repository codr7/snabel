(defpackage clvm
  (:use cl)
  (:export *version* form op pos val-type val))

(in-package clvm)

(define-symbol-macro *version* 1)

(defvar *vm*)

(defstruct pos
  (source "n/a" :type string)
  (row -1 :type integer)
  (col -1 :type integer))

(defvar *default-pos* (make-pos))

(defstruct form
  (pos *default-pos* :type pos))

(defvar *default-form* (make-form :pos *default-pos*))

(defstruct op
  (form *default-form* :type form))

(defstruct lib
  (name (error "Missing name") :type keyword)
  (bindings (make-hash-table)))

(defstruct vm-type
  (name (error "Missing name") :type keyword))

(defstruct (val (:conc-name))
  (vm-type (error "Missing type") :type vm-type)
  (data (error "Missing data") :type t))

(defun val (type data)
  (make-val :vm-type type :data data))
