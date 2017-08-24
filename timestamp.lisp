(defun time-to-utc (u-time)
  "Convert universal time to UTC equivalent on this machine"
  (multiple-value-bind (sec min hr day mon yr wkd dst tz)
      (decode-universal-time u-time)
    (declare (ignore sec min hr day mon yr wkd))
    (+ u-time (* (- tz (if dst 1 0)) (* 60 60)))))

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
