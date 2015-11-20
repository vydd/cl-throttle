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

(defun chain (first-throttle second-throttle &rest rest)
  (let ((throttles (make-array (+ 2 (length rest))
			       :initial-contents (nconc
						  `(,first-throttle
						    ,second-throttle) rest))))
    (lambda (state)
      (let* ((state (or state (make-array (length throttles)
					  :initial-element nil)))
	     (chained-result t))
	(loop
	   while chained-result
	   for idx from 0 below (length throttles)
	   for throttle across throttles
	   do
	     (multiple-value-bind (result next-state)
		 (funcall throttle (aref state idx))
	       (setf chained-result result
		     (aref state idx) next-state)))
	(values chained-result
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
