(in-package :common-lisp-user)
(load "C:/Users/Darren/quicklisp/setup.lisp")
(ql:quickload :flexi-streams)
(ql:quickload :chipz)
(ql:quickload :salza2)

(defpackage :freezip
  (:use :common-lisp
	:flexi-streams
	:chipz
	:salza2)
  (:export :compress-string
	   :decompress-string))

(in-package :freezip)

(defun compress-octets (octet-vector)
  (coerce
   (flexi-streams:with-output-to-sequence (outs)
     (salza2:with-compressor
	 (c 'salza2:gzip-compressor
	    :callback (salza2:make-stream-output-callback outs))
       (salza2:compress-octet-vector octet-vector c)))
   '(simple-array (unsigned-byte 8) (*))))

(defun compress-string (string)
  (compress-octets (string-to-octets string)))

(defun decompress-octets (byte-array)
  (chipz:decompress nil 'chipz:gzip byte-array))

(defun decompress-string (byte-array)
  (octets-to-string (decompress-octets byte-array)))
