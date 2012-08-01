(defun sv-log (&rest args)
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz) (get-decoded-time)
    (declare (ignore tz dst-p day-of-week))
    (format t "sv-log: ~a-~2,'0d-~2,'0d_~2,'0d:~2,'0d:~2,'0d: " year month date hour minute second))
  (apply #'format t args))

