(load "C:/Users/Darren/quicklisp/setup.lisp")

(ql:quickload :cl-base64)
(ql:quickload :ironclad)
(ql:quickload :sqlite)
(ql:quickload :split-sequence)

(load "freezip")


(defpackage :file-cabinet
  (:use :common-lisp
	:sb-ext
	:sqlite
	:freezip)
  (:export ))
(in-package :file-cabinet)

(load "timestamp")

(defun string-replace-character (string character replacement)
  (do ((string-list (coerce string 'list))
       (i 0 (+ i 1))
       (new-string-list nil))
      ((= i (length string-list))
       (reverse (coerce new-string-list 'string)))
    (let ((c (elt string-list i)))
      (if (eql c character)
	  (push replacement new-string-list)
	  (push c new-string-list)))))    

(defun read-file-octets (filename)
  (coerce
   (with-open-file (inf filename
			:direction :input
			:element-type '(unsigned-byte 8))
     (do ((l nil)
	  (c (read-byte inf nil nil) (read-byte inf nil nil)))
	 ((null c) (reverse l))
       (push c l)))		  
   '(simple-array (unsigned-byte 8) (*))))

(defun read-file-octets-with-optional-mutex (filename &optional mutex)
  (if (null mutex)
      (read-file-octets filename)
      (sb-thread:with-mutex (mutex)
	(read-file-octets filename))))

(defun md5-hash-file-contents (contents
			       &key
				 from-octets?
				 (file-encoding :ascii))
  (base64:string-to-base64-string
   (map 'string #'code-char
	(ironclad:digest-sequence :md5
				  (if from-octets?
				      contents
				      (string-to-octets
				       contents
				       :external-format file-encoding))))))

(defun crc32-hash-file-contents (contents
				 &key
				   from-octets?
				   (file-encoding :ascii))
  (base64:string-to-base64-string
   (map 'string #'code-char
	(ironclad:digest-sequence :crc32
				  (if from-octets?
				      contents
				      (string-to-octets
				       contents
				       :external-format file-encoding))))))

(defun save-current-file-contents (filename cabinet-filename &optional mutex)
  (let ((data (read-file-octets-with-optional-mutex filename mutex)))
    (with-open-database (db cabinet-filename)
      (execute-non-query db
			 "INSERT INTO FileContents(filename, 
                                                   timestamp,
                                                   md5,
                                                   crc32, 
                                                   contents)
                               VALUES (?, ?, ?, ?, ?)"
			 filename
			 (iso-time t)
			 (md5-hash-file-contents data :from-octets? t)
			 (crc32-hash-file-contents data :from-octets? t)
			 (freezip::compress-octets data)))))

(defun default-cabinet-name-from-file (filename)
  (format nil "~A.lfc" (string-replace-character filename #\. #\_)))

(defun make-cabinet-from-file (filename &key cabinet-filename mutex)
  (when (null cabinet-filename)
    (setf cabinet-filename (default-cabinet-name-from-file filename)))
  (with-open-database (db cabinet-filename)
    (execute-non-query db
		       "CREATE TABLE FileContents(
                                       id INTEGER PRIMARY KEY AUTOINCREMENT,
                                       filename TEXT NOT NULL,
                                       timestamp TEXT NOT NULL,
                                       md5 TEXT NOT NULL,
                                       crc32 TEXT NOT NULL,
                                       contents BLOB NOT NULL)"))
  (save-current-file-contents filename cabinet-filename mutex)
  cabinet-filename)

(defun get-latest-version (cabinet-filename)
  (freezip::decompress-string
   (with-open-database (db cabinet-filename)
     (execute-single db
		     "SELECT contents FROM FileContents
                       WHERE timestamp = (SELECT MAX(timestamp) 
                                            FROM FileContents)"))))

(defun get-most-recent-filename (cabinet-filename)
  (with-open-database (db cabinet-filename)
    (execute-single db
		    "SELECT filename FROM FileContents
                      WHERE timestamp = (SELECT MAX(timestamp)
                                           FROM FileContents)")))

(defun clear-current-file (cabinet-filename &optional mutex)
  (let ((filename (get-most-recent-filename cabinet-filename)))
    (labels ((clear-file ()
		 (with-open-file (f filename
				    :direction :output
				    :if-exists :supersede))))
		 
      (if (not (null mutex))
	  (sb-thread:with-mutex (mutex) (clear-file))
	  (clear-file)))))

(defun save-current-version (cabinet-filename &key filename mutex clear)
  (when (null filename)
    (setf filename (get-most-recent-filename cabinet-filename)))
  (save-current-file-contents filename cabinet-filename mutex)
  (when clear (clear-current-file cabinet-filename)))

(defun get-saved-versions (cabinet-filename &key saved-before saved-after)
  (with-open-database (db cabinet-filename)
    (when (null saved-before)
      (setf saved-before (iso-time t)))
    (when (null saved-after)
      (setf saved-after (iso-universal-time 0)))
    (mapcar #'(lambda (x) (freezip::decompress-string (car x)))
	    (execute-to-list/named db
				   "SELECT contents FROM FileContents
                                     WHERE timestamp > :after
                                       AND timestamp < :before"
				   ":after" saved-after
				   ":before" saved-before))))

(defun file-contents-to-lines (contents &key remove-empty?)
  (let ((lines (split-sequence:split-sequence #\Newline contents)))
    (if remove-empty?
	(remove-if #'(lambda (x) (eql (length x) 0)) lines)
	lines)))

(defun get-joined-archive-lines (cabinet-filename
				 &key
				   saved-before
				   saved-after
				   first-line-redundant?)
  (let* ((versions (get-saved-versions cabinet-filename
				       :saved-before saved-before
				       :saved-after saved-after))
	 (version-lines
	  (mapcar #'(lambda (x) (file-contents-to-lines x :remove-empty? t))
		  versions)))
    (apply #'concatenate
	   (cons 'list
		 (cons (car version-lines)
		       (if first-line-redundant?
			   (mapcar #'cdr version-lines)
			   version-lines))))))

(defun restore-file (cabinet-filename &key mutex)
  (let* ((filename (get-most-recent-filename cabinet-filename))
	 (contents (get-latest-version cabinet-filename)))
    (labels ((restore-filename ()
	       (with-open-file (f filename
				  :direction :output
				  :if-exists :supersede)
		 (write-sequence contents f))))
      (if (not (null mutex))
	  (sb-thread:with-mutex (mutex) (restore-filename))
	  (restore-filename)))))

