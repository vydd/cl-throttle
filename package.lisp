;;;; package.lisp

(defpackage #:cl-throttle
  (:use #:cl)
  (:nicknames :throttle)
  (:export :limit
	   :once
	   :interval
	   :throttle-and
	   :throttle-or
	   :with-throttle
	   :defun-throttled
	   :reset-throttle-state))

