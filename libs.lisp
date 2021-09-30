(in-package snabl)

(defclass lib ()
  ((name :initform (error "Missing name") :reader name)
   (bindings :initform (make-hash-table) :reader bindings)))

(defun lib-find (lib key)
  (gethash key (bindings lib)))

(defun (setf lib-find) (val lib key)
  (setf (gethash key (bindings lib)) val))

(defun lib-bind (lib key vm-type data)
  (setf (lib-find lib key) (new-val vm-type data)))

(defun lib-import (lib &rest keys)
  (if keys
      (dolist (k keys)
	(let ((v (lib-find lib k)))
	  (unless v
	    (error "Unknown id: ~a" k))
	  (setf (scope-find k) v)))
      
      (dohash (k v (bindings lib))
	(setf (scope-find k) v))))

(defmacro lib-bind-type (lib type parent-types)
  `(progn
     (lib-bind ,lib (name (,type ,lib)) (meta-type *abc-lib*) (,type ,lib))
     
     ,@(mapcar (lambda (pt)
		 `(derive (,type ,lib) (,pt ,lib)))
	       parent-types)))

(defmacro lib-bind-func (lib name (&rest args) (&rest rets) body)
  (flet ((parse-arg (arg)
	   `(cons ,(first arg) ,(second arg))))
    `(lib-bind ,lib
	       ,name
	       (func-type *abc-lib*)
	       (new-func ,name
			 (make-array ,(length args) :initial-contents (list ,@(mapcar #'parse-arg args)))
			 (make-array ,(length rets) :initial-contents (list ,@rets))
			 ,body))))
  
(defun lib-bind-prim (lib name arg-count body)
  (lib-bind lib name (prim-type *abc-lib*) (new-prim name arg-count body)))

;; abc

(defclass any-type (vm-type)
  ((name :initform :|Any|)))

(defclass bool-type (vm-type)
  ((name :initform :|Bool|)
   (val-dump :initform (lambda (v out) (write-string (if v "T" "F") out)))
   (val-is-true? :initform (lambda (v) v))))

(defclass func-type (vm-type)
  ((name :initform :|Func|)))

(defclass int-type (vm-type)
  ((name :initform :|Int|)
   (val-is-true? :initform (lambda (v) (not (zerop v))))))

(defclass meta-type (vm-type)
  ((name :initform :|Meta|)))

(defclass prim-type (vm-type)
  ((name :initform :|Prim|)))

(defclass reg-type (vm-type)
  ((name :initform :|Reg|)))

(defclass target-type (vm-type)
  ((name :initform :|Target|)))

(defclass abc-lib (lib)
  ((name :initform :abc)
   (any-type :initform (make-instance 'any-type :name :|Any|) :reader any-type)
   (bool-type :initform (make-instance 'bool-type) :reader bool-type)
   (func-type :initform (make-instance 'func-type) :reader func-type)
   (int-type :initform (make-instance 'int-type) :reader int-type)
   (meta-type :initform (make-instance 'meta-type) :reader meta-type)
   (prim-type :initform (make-instance 'prim-type) :reader prim-type)
   (reg-type :initform (make-instance 'reg-type) :reader reg-type)
   (target-type :initform (make-instance 'target-type) :reader target-type)))

(defmethod init ((self lib))
  (declare (ignore args))

  (lib-bind-type self any-type ())
  (lib-bind-type self target-type (any-type))

  (lib-bind-type self bool-type (any-type))
  (lib-bind-type self func-type (any-type target-type))
  (lib-bind-type self int-type (any-type))
  (lib-bind-type self meta-type (any-type))
  (lib-bind-type self prim-type (any-type target-type))
  (lib-bind-type self reg-type (any-type))

  (lib-bind-prim self :|cp| 0 (lambda (self f in)
				(emit-op (new-copy-op :form f))
				in))

  (lib-bind-func self :|dump|
		 ((:|value| (any-type *abc-lib*)))
		 ()
		 (lambda (self pos ret-label)
		   (dump (vm-pop))
		   (terpri)
		   ret-label))

  (lib-bind-prim self :|if| 3 (lambda (self f in)
				(let ((cnd (pop in))
				      (t-body (pop in))
				      (f-body (pop in))
				      (f-label (gensym))
				      (end-label (gensym)))
				  (form-emit cnd nil)
				  (emit-op (new-branch-op f-label :form f))
				  (form-emit t-body nil)
				  (emit-op (new-goto-op end-label :form f))
				  (emit-op (new-label-op f-label :form f))
				  (form-emit f-body nil)
				  (emit-op (new-label-op end-label :form f)))
				in))

  (lib-bind-prim self :|let| 2 (lambda (self f in)
				 (let* ((kf (pop in))
					(vf (pop in))
					(v (form-val vf)))
				   (if v
				       (scope-bind (id-name kf) v)
				       (progn
					 (setf in (form-emit vf in))
					 (emit-op (new-store-op (scope-bind-reg (id-name kf)))))))
				 in))

  (lib-bind self :T (bool-type self) t)
  (lib-bind self :F (bool-type self) nil))
