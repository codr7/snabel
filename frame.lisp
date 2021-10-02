(in-package snabl)

(defclass frame (proc)
  ((pos :initform (error "Missing pos") :initarg :pos :reader pos)
   (func :initform (error "Missing func") :initarg :func :reader func)))

(defun new-frame (pos func)
  (make-instance 'frame :pos pos :func func))

(defmethod capture ((self frame))
  (with-slots (func scope stack) self
    (setf scope (body-scope func))
    (setf stack (copy-vector (subseq *stack* (- (length *stack*) (length (args func))))))
    (setf (slot-value *proc* 'stack) (copy-vector (subseq *stack* 0 (- (length *stack*) (length (args func))))))))

(defmethod restore ((self frame))
  (with-slots (func stack) self
    (let ((rets (subseq stack (- (length stack) (length (rets func))))))
      (dovector (v rets)
	(vm-push v)))))
 
(defun push-frame (frame &key (vm *vm*))
  (push-proc frame :vm vm)
  (with-slots (frames frame-cache) vm
    (vector-push-extend frame frames)
    (setf frame-cache frame)))

(defun pop-frame (&key (vm *vm*))
  (with-slots (frames frame-cache) vm
    (vector-pop frames)
    (let ((n (length frames)))
      (setf frame-cache (unless (zerop n) (aref frames (1- n))))))
  (pop-proc :vm vm))
