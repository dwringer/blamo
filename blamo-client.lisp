#-quicklisp (load "C:/Users/Darren/quicklisp/setup.lisp")
(ql:quickload :drakma)

(in-package :common-lisp)
(defpackage :blamo-client
  (:use :common-lisp)
  (:export :log-append
	   :log-create))
(in-package :blamo-client)

(defparameter *host-ip* "10.10.12.124")


(defun make-request (operation parameters
		     &key timeout (port 9003) connection-timeout)
  (if timeout
      (sb-ext:with-timeout timeout
	(drakma:http-request
	 (format nil
		 "http://~A:~A/~A"
		 *host-ip* port operation)
	 :parameters parameters
	 :connection-timeout connection-timeout))
      (drakma:http-request
       (format nil
	       "http://~A:~A/~A"
	       *host-ip* port operation)
       :parameters parameters
       :connection-timeout connection-timeout)))


(defun log-create (identifier &key (port 41111))
  (make-request "log-create"
		(list (cons "id" (format nil "\"~A\"" identifier)))
		:port port))


(defun log-append (identifier message &key (port 41111))
  (make-request "log-append"
		(list (cons "id" (format nil "\"~A\"" identifier))
		      (cons "message" (format nil "\"~A\"" message)))
		:port port))
