;;;; package.lisp

(defpackage #:cl-throttle
  (:use #:cl)
  (:nicknames :throttle)
  (:export :limit
	   :once
	   :interval
	   :chain
	   :with-throttle
	   :defun-throttled
	   :reset-throttle-state))

