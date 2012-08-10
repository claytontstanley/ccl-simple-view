(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cocoa)
  (require :easygui))

(defun sv-log (&rest args)
  (#_NSLog 
   (objc:make-nsstring
     (multiple-value-bind (second minute hour date month year day-of-week dst-p tz) (get-decoded-time)
       (declare (ignore tz dst-p day-of-week))
       (with-output-to-string (strm) 
         (format strm "sv-log: ~a-~2,'0d-~2,'0d_~2,'0d:~2,'0d:~2,'0d: " year month date hour minute second)
         (unwind-protect (apply #'format strm args)
           (fresh-line strm)))))))
