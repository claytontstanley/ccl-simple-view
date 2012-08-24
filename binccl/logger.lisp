(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cocoa)
  (require :easygui))

(defparameter *sv-log-level* 1)

(defun sv-log (&rest args)
  (#_NSLog 
   (objc:make-nsstring
     (multiple-value-bind (second minute hour date month year day-of-week dst-p tz) (get-decoded-time)
       (declare (ignore tz dst-p day-of-week))
       (with-output-to-string (strm) 
         (format strm "sv-log: ~a-~2,'0d-~2,'0d_~2,'0d:~2,'0d:~2,'0d: on thread ~a: " year month date hour minute second *current-process*)
         (unwind-protect (apply #'format strm args)
           (fresh-line strm)))))))

(defun sv-log-n (log-level &rest args)
  (when (<= log-level *sv-log-level*)
    (apply #'sv-log args)))

