(in-package :common-lisp-user)
(load "C:/Users/Darren/quicklisp/setup.lisp")
(ql:quickload :clack)
(ql:quickload :snooze)

(defpackage :blamo
  (:use :common-lisp
	:sb-ext
	:clack
	:snooze)
  (:shadowing-import-from :snooze
			  :stop)
  (:export :create-log
	   :list-logs
	   :write-to-log
	   :read-from-log
	   :start-server
	   :stop-server))
(in-package :blamo)


;;; FROM Edi Weitz's CL-FAD, based on Peter Sebel's _Practical Common Lisp_: --
(defun component-present-p (value)
  "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
   is neither NIL nor the keyword :UNSPECIFIC."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC \(a pathname designator) does not designate
a directory, PATHSPEC otherwise.  It is irrelevant whether file or
directory designated by PATHSPEC does actually exist."
  (and
   (not (component-present-p (pathname-name pathspec)))
   (not (component-present-p (pathname-type pathspec)))
   pathspec))

(defun file-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and returns its truename if this is the case, NIL otherwise.
The truename is returned in `canonical' form, i.e. the truename of a
directory is returned as if by PATHNAME-AS-DIRECTORY."
  (probe-file pathspec))

(defun directory-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and if it is a directory.  Returns its truename if this is the
case, NIL otherwise.  The truename is returned in directory form as if
by PATHNAME-AS-DIRECTORY."
  (let ((result (file-exists-p pathspec)))
    (and result
         (directory-pathname-p result)
         result)))
;;; END CL-FAD EXCERPT --------------------------------------------------------


;;; TIMESTAMP FUNCTIONS: ------------------------------------------------------
(defun time-to-utc (u-time)
  "Convert universal time to UTC equivalent on this machine"
  (multiple-value-bind (sec min hr day mon yr wkd dst tz)
      (decode-universal-time u-time)
    (declare (ignore sec min hr day mon yr wkd))
    (+ u-time  ; (get-universal-time)
       (* (- tz (if dst 1 0)) (* 60 60)))))

(defun iso-universal-time (u-time)
  "Make a given universal time into an ISO formatted string"
  (multiple-value-bind (sec min hr day mon yr wkd dst tz)
      (decode-universal-time u-time)
    (declare (ignore wkd dst tz))
    (let ((mdhms (list mon day hr min sec)))
      (multiple-value-bind (mon day hr min sec)
	  (apply #'values (mapcar #'(lambda (x) (format nil "~2,'0d" x)) mdhms))
	(reduce #'(lambda (a b) (format nil "~A~A" a b))
		(list yr "-" mon "-" day "T" hr ":" min ":" sec))))))

(defun get-utc-time ()
  "Return universal time as UTC"
  (time-to-utc (get-universal-time)))

(defun iso-time (&optional (utc? nil))
  "Return an ISO time string, optionally in UTC"
  (let ((time (get-universal-time)))
    (iso-universal-time (if utc? (time-to-utc time) time))))
;;; END TIMESTAMP FUNCTION DEFINITIONS ----------------------------------------


(defparameter *default-log-configuration* #P"C:/DWR/Main/blamo.dat")

(defparameter *file-io-mutex* (sb-thread:make-mutex))

(defun read-lines-from-file (filename)
  "Read all lines of text from a given file"
  (with-open-file (inf filename :direction :input :if-does-not-exist nil)
    (when inf
      (do (lines
	   (next (read-line inf nil nil) (read-line inf nil nil)))
	  ((null next) (reverse lines))
	(push next lines)))))

(defun read-sexp-from-file (filename)
  "Read an s-expression that was written to a file"
  (let (result)
    (with-open-file (inf filename :direction :input :if-does-not-exist nil)
      (when (not (null inf)) (setf result (read inf nil nil))))
    result))

(defun write-sexp-to-file (obj filename)
  "Write an object to a file (only s-expressions supported for reading)"
  (with-open-file (outf filename :direction :output :if-exists :supersede)
    (write obj :stream outf)))

(defun load-log-data (&optional (from-file *default-log-configuration*))
  "Load the hashtable of known log identifiers mapped to log filenames"
  (let ((log-files-hash-table (make-hash-table :test 'equal))
	(log-names-to-filenames-alist (read-sexp-from-file from-file)))
    (map nil #'(lambda (name filename)
		 (setf (gethash name log-files-hash-table) filename))
	 (mapcar #'car log-names-to-filenames-alist)
	 (mapcar #'cdr log-names-to-filenames-alist))
    log-files-hash-table))

(defparameter *log-data* (load-log-data))

(defun list-logs ()
  "Returns an associative list of log identifiers to log filenames"
  (let (result)
    (with-locked-hash-table (*log-data*)
      (maphash #'(lambda (k v) (push (cons k v) result)) *log-data*))
    result))

(defun save-log-data (&optional (to-file *default-log-configuration*))
  "Records the log identifier hashtable to a file"
  (sb-thread:grab-mutex *file-io-mutex*)
  (write-sexp-to-file (list-logs) to-file)
  (sb-thread:release-mutex *file-io-mutex*))

(defparameter *default-log-directory* #P"C:/DWR/Logs/blamo/")

(define-condition logfile-exists-error (error)
  ((text :initarg :text :reader text)))

(defun create-log (identifier
		   &optional
		     filename
		     (folder *default-log-directory*))
  "Create and register a log file under the given identifier"
  (when (null filename)
    (setf filename (format nil "~A.log" identifier)))
  (let ((full-path (merge-pathnames filename folder)))
    (if (not (file-exists-p full-path))
	(progn
	  (sb-thread:grab-mutex *file-io-mutex*)
	  (ensure-directories-exist folder)
	  (with-open-file (outf full-path
				:direction :output
				:if-does-not-exist :create
				:if-exists :append)
	    (format outf "log ~A begin~%~&" identifier))
	  (sb-thread:release-mutex *file-io-mutex*)
	  (with-locked-hash-table (*log-data*)
	    (setf (gethash identifier *log-data*) full-path))
	  (save-log-data))
	(error 'logfile-exists-error :text (format nil "~A" full-path)))))

(defun write-to-log (identifier message level)
  "Write a message at the specified level to the specified log"
  (let ((output-filename
	 (with-locked-hash-table (*log-data*)
	   (gethash identifier *log-data*)))
	(output-line (format nil "~A> [~A] ~A~%~&"
			     level (iso-time t) message)))
    (sb-thread:grab-mutex *file-io-mutex*)
    (with-open-file (outf output-filename
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :append)
      (format outf output-line))
    (sb-thread:release-mutex *file-io-mutex*)))

(defun read-from-log (identifier &optional levels)
  "Collect lines from the specified log, optionally filtered by level"
  (let ((input-filename (with-locked-hash-table (*log-data*)
			  (gethash identifier *log-data*))))
    (remove-if #'null
	       (mapcar #'(lambda (l)
			   (let* ((lst (coerce l 'list))
				  (break-point (position #\> lst))
				  (level (coerce (subseq lst 0 break-point)
						 'string)))
			     (when (or (null levels)
				       (member level levels :test 'string=))
			       l)))
		       (read-lines-from-file input-filename)))))


;;; HTTP API ROUTES: ----------------------------------------------------------
(snooze:defroute log-create (:get :text/* &key id)
  (let ((success t))
    (handler-case (create-log id)
      (logfile-exists-error (e)
	(declare (ignore e))
	(setf success nil)))
    (if success
	(http-condition 200 "OK")
	(http-condition 422))))

(snooze:defroute log-append (:get :text/* &key id (message ""))
  (progn
    (write-to-log id message "DEBUG")
    (http-condition 200 "OK")))
;;; END ROUTE DEFINITIONS -----------------------------------------------------


;;; HTTP SERVER CONTROL: ------------------------------------------------------
(defparameter *handler* nil)

(defmacro start-server (&key (handler '*handler*) (port 41111))
  "Initialize an HTTP handler"
  `(setf ,handler
	 (clack:clackup
	  (snooze:make-clack-app)
	  :port ,port)))

(defmacro stop-server (&key (handler '*handler*))
  "Shutdown the HTTP handler"
  `(clack:stop ,handler))
;;; END SERVER CONTROL --------------------------------------------------------
