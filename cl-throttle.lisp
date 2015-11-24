;;;; cl-throttle.lisp

(in-package #:cl-throttle)

(defun limit (n)
  (lambda (limit)
    (let* ((limit (or limit n))
	   (above-limit (plusp limit)))
      (values above-limit
	      (if above-limit
		  (- limit 1)
		  0)))))

(defun once ()
  (limit 1))

(defun interval (ms)
  (let ((internal-interval
	 (* ms (/ internal-time-units-per-second 1000))))
    (lambda (previous-time)
      (let* ((previous-time (or previous-time 0))
	     (current-time (get-internal-real-time))
	     (diff (- current-time previous-time))
	     (interval-passed (> diff internal-interval)))
	(values interval-passed
		(if interval-passed
		    current-time
		    previous-time))))))

(defun throttle-and (first-throttle second-throttle &rest rest)  
  (let* ((throttles-list (nconc `(,first-throttle ,second-throttle) rest))
	 (throttles (make-array (+ 2 (length rest)) :initial-contents throttles-list)))
    (lambda (state)
      (let* ((state (or state (make-array (length throttles) :initial-element nil)))
	     (and-result t))
	(loop
	   while and-result
	   for idx below (length throttles)
	   for throttle across throttles
	   do
	     (multiple-value-bind (result next-state)
		 (funcall throttle (aref state idx))
	       (setf and-result result
		     (aref state idx) next-state)))
	(values and-result
		state)))))

(defun make-throttle-or-state (throttles)
  (make-array (length throttles)
	      :initial-contents
	      (loop
		 for i below (length throttles)
		 for throttle across throttles
		 collect
		   (multiple-value-bind (_ state)
		       (funcall throttle nil)
		     (declare (ignore _))
		     (if (zerop i)
			 nil
			 state)))))

(defun throttle-or (first-throttle second-throttle &rest rest)
  (let* ((throttles-list (nreverse (nconc `(,first-throttle ,second-throttle) rest)))
	 (throttles (make-array (+ 2 (length rest)) :initial-contents throttles-list)))
    (lambda (state)
      (let* ((state (or state (make-throttle-or-state throttles)))
	     (or-result nil))
	(loop
	   while (not or-result)
	   for idx below (length throttles)
	   for throttle across throttles
	   do
	     (multiple-value-bind (result next-state)
		 (funcall throttle (aref state idx))
	       (setf or-result result
		     (aref state idx) next-state))
	   finally
	     (when or-result
	       (loop
		  for i below idx
		  do
		    (setf (aref state i) (multiple-value-bind (_ state)
					     (funcall (aref throttles i) nil)
					   (declare (ignore _))
					   state)))))
	(values or-result
		state)))))

(defparameter *throttle-states* (make-hash-table))

(defmacro with-throttle (throttle &body body)
  (let ((state (gensym "THROTTLE-STATE"))
	(test (gensym "TEST"))
	(new-state (gensym "NEW-STATE")))
    `(multiple-value-bind (,test ,new-state)
	 (funcall ,throttle (gethash ',state *throttle-states* nil))
       (setf (gethash ',state *throttle-states*) ,new-state)
       (values (if ,test
		   (progn
		     ,@body)
		   nil)
	       ,test
	       ',state))))

(defmacro defun-throttled (throttle spec args &body body)
  `(defun ,spec ,args
     (with-throttle ,throttle
       ,@body)))

(defun reset-throttle-state (throttle)
  (setf (gethash throttle *throttle-states*) nil))
