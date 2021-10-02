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
	  (scope-bind k v)))
      (dohash (k vv (bindings lib))
	(scope-bind k vv))))

(defmacro lib-bind-type (lib type parent-types)
  `(progn
     (lib-bind ,lib (name (,type ,lib)) (meta-type *abc-lib*) (,type ,lib))
     
     ,@(mapcar (lambda (pt)
		 `(derive (,type ,lib) (,pt ,lib)))
	       parent-types)))

(defmacro lib-bind-func (lib name (&rest args) (&rest rets) body)
  (flet ((parse-arg (arg)
	   `(new-arg ,(first arg) ,(second arg))))
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
   (val-dump :initform (lambda (v out)
			 (write-char (if v #\T #\F) out)))))

(defclass func-type (vm-type)
  ((name :initform :|Func|)))

(defclass int-type (vm-type)
  ((name :initform :|Int|)
   (val-is-true? :initform (lambda (v)
			     (not (zerop v))))))

(defclass meta-type (vm-type)
  ((name :initform :|Meta|)))

(defclass prim-type (vm-type)
  ((name :initform :|Prim|)))

(defclass reg-type (vm-type)
  ((name :initform :|Reg|)))

(defclass sym-type (vm-type)
  ((name :initform :|Sym|)
   (val-dump :initform (lambda (v out)
			 (format out "'~a" (symbol-name v))))))

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
   (sym-type :initform (make-instance 'sym-type) :reader sym-type)
   (target-type :initform (make-instance 'target-type) :reader target-type)))

(defmethod init ((self abc-lib))
  (lib-bind-type self any-type ())
  (lib-bind-type self target-type (any-type))

  (lib-bind-type self bool-type (any-type))
  (lib-bind-type self func-type (any-type target-type))
  (lib-bind-type self int-type (any-type))
  (lib-bind-type self meta-type (any-type))
  (lib-bind-type self prim-type (any-type target-type))
  (lib-bind-type self reg-type (any-type))
  (lib-bind-type self sym-type (any-type))

  (lib-bind-prim self :|cp| 0 (lambda (self f in)
				(emit-op (new-copy-op :form f))
				in))

  (lib-bind-func self :|dump|
      ((:|value| (any-type *abc-lib*)))
      ()
      (lambda (self pos)
	(dump (vm-pop))
	(terpri)))

  (lib-bind-prim self :|func| 4 (lambda (self f in)
				  (let ((name (pop in))
					(args (pop in))
					(rets (pop in))
					(body (pop in)))
				    (flet ((parse-args (in)
					     (let* ((arg-count (floor (length (group-body args)) 2))
						    (out (make-array arg-count :initial-element nil))
						    (i 0))
					       (labels ((rec (in)
							  (when in
							    (let* ((name (pop in))
								   (type-name (pop in))
								   (vm-type (scope-find (id-name type-name))))
							      (unless vm-type
								(e-emit (form-pos f) "Unknown type: ~a" type-name))
							      (setf (aref out i) (new-arg (id-name name) (data vm-type))))
							    (incf i)
							    (rec in))))
						 (rec in))
					       out))
					   (parse-rets (in)
					     (let* ((ret-count (length (group-body rets)))
						    (out (make-array ret-count :initial-element nil))
						    (i 0))
					       (labels ((rec (in)
							  (when in
							    (let* ((type-name (pop in))
								   (vm-type (scope-find (id-name type-name))))
							      (unless vm-type
								(e-emit (form-pos f) "Unknown type: ~a" type-name))
							      (setf (aref out i) (data vm-type)))
							    (incf i)
							    (rec in))))
						 (rec in))
					       out)))
				      
				      (let ((func (new-func (id-name name)
							    (parse-args (group-body args))
							    (parse-rets (group-body rets))
							    nil)))
					(scope-bind (name func) (new-val (func-type *abc-lib*) func))
					(func-emit func body in))))))

  (lib-bind-prim self :|if| 3 (lambda (self f in)
				(setf in (form-emit (pop in) in))
				
				(let ((f-label (gensym))
				      (end-label (gensym)))
				  (emit-op (new-branch-op f-label :form f))
				  (setf in (form-emit (pop in) in))
				  (emit-op (new-goto-op end-label :form f))
				  (emit-op (new-label-op f-label :form f))
				  (setf in (form-emit (pop in) in))
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

;; math

(defclass math-lib (lib)
  ((name :initform :math)))

(defmethod init ((self math-lib))
  (lib-bind-func self :<
      ((:|x| (any-type *abc-lib*)) (:|y| (any-type *abc-lib*)))
      ((bool-type *abc-lib*))
      (lambda (self pos)
	(let* ((y (vm-pop)) (x (vm-peek))
	       (result (compare x y)))
	  (setf (vm-type x) (bool-type *abc-lib*)
		(data x) (eq result :lt)))))

  (lib-bind-func self :>
      ((:|x| (any-type *abc-lib*)) (:|y| (any-type *abc-lib*)))
      ((bool-type *abc-lib*))
      (lambda (self pos)
	(let* ((y (vm-pop)) (x (vm-peek))
	       (result (compare x y)))
	  (setf (vm-type x) (bool-type *abc-lib*)
		(data x) (eq result :gt)))))

  (lib-bind-func self :+
      ((:|x| (int-type *abc-lib*)) (:|y| (int-type *abc-lib*)))
      ((int-type *abc-lib*))
      (lambda (self pos)
	(let ((y (vm-pop)) (x (vm-peek)))
	  (incf (data x) (data y)))))

  (lib-bind-func self :-
      ((:|x| (int-type *abc-lib*)) (:|y| (int-type *abc-lib*)))
      ((int-type *abc-lib*))
      (lambda (self pos)
	(let ((y (vm-pop)) (x (vm-peek)))
	  (decf (data x) (data y))))))

