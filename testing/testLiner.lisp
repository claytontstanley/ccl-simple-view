(clear-all)
(define-model foo ())
(setf *win* (make-instance 'rpm-real-window))
(apply #'add-visual-items-to-rpm-window *win*
       (loop for (start-point end-point) in (group
                                              (list '(0 0) '(10 10)
                                                    '(20 40) '(40 20)
                                                    '(60 20) '(40 20)
                                                    '(80 20) '(60 10))
                                              2)
             collect (make-line-for-rpm-window *win* start-point end-point)))
(install-device *win*)
(check
  (equal (list (list 0 10) (list 0 10) (list 40 20) (list 20 40) (list 20 20) (list 40 60) (list 10 20) (list 60 80))
         (let ((loc-avg ()))
           (with-shadow (loc-avg (lambda (x y)
                                   (push (list x y) loc-avg)
                                   (funcall fun-orig x y)))
             (proc-display))
           loc-avg)))
(check
  (equal '((5 5) (30 30) (50 20) (70 15))
         (mapcar (lambda (chunk)
                   (list 
                     (chunk-slot-value-fct  chunk 'screen-x) 
                     (chunk-slot-value-fct  chunk 'screen-y)))
                 (visicon-chunks (get-module :vision) t))))

