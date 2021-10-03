(in-package snabl)

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(defstruct (frame (:include proc))
  (pos (error "Missing pos"))
  (func (error "Missing func")))

(defun new-frame (pos func)
  (make-frame :pos pos :func func))

(defmethod capture ((self frame))
  (with-slots (func regs scope stack) self
    (dotimes (i (min-reg func))
      (setf (aref regs i) (aref *regs* i)))
    
    (setf scope (body-scope func))
    (setf stack (stack-copy *stack* :start (- (length *stack*) (length (args func)))))
    (setf (slot-value *proc* 'stack) (stack-copy *stack* :start 0 :end (- (length *stack*) (length (args func)))))))

(defmethod restore ((self frame) &key drop-rets?)
  (when drop-rets?
    (return-from restore))
  
  (with-slots (func stack) self
    (dotimes (i (length (rets func)))
      (vm-push (aref stack (+ (- (length stack) (length (rets func))) i))))))
 
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
