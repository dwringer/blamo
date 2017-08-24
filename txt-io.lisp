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

