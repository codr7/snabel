(in-package snabl)

(defclass vm-type ()
  ((id :initform (next-type-id) :reader id)
   (name :initform (error "Missing name") :initarg :name :reader name)
   (parent-types :initform (make-array *max-type-count* :element-type 'boolean :initial-element nil)
		 :reader parent-types)
   (val-clone :initform #'identity
	      :reader val-clone)
   (val-compare :initform #'compare
		:reader val-compare)
   (val-dump :initform (lambda (v out)
			 (print-object v out))
	     :reader val-dump)
   (val-is-true? :initform (lambda (v)
			     v)
		 :reader val-is-true?)))

(defmethod print-object ((self vm-type) out)
  (write-string (symbol-name (name self)) out))

(defun derive (child parent)
  (setf (aref (parent-types child) (id parent)) t))

(defun isa (child parent)
  (aref (parent-types child) (id parent)))

(defmethod initialize-instance :after ((self vm-type) &rest args &key &allow-other-keys)
  (derive self self))
