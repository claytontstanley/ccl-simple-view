; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defun make-temp-file ()
  (string-right-trim
    (list #\newline)
    (with-output-to-string (strm)
      (run-program "mktemp" (list "/tmp/lisp-file-locks-XXXX") :output strm))))

(setf *temp-file* (make-temp-file))
(check (not (file-locked-p *temp-file*)))
(check (delete-file *temp-file*))
(setf *temp-file* (make-temp-file))
(lock-file *temp-file*)
(check (file-locked-p *temp-file*))
(check (errors-p (delete-file *temp-file*)))
(unlock-file *temp-file*)
(check (not (file-locked-p *temp-file*)))
(check (delete-file *temp-file*))
