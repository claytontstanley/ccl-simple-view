; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(make-instance
  'window
  :view-subviews
  (append
    (loop for y from 0 by 20 upto 100
          collect (make-instance
                    'radio-button-dialog-item
                    :view-nick-name (+ y 0)
                    :view-position (make-point 10 y)))
    (loop for y from 0 by 20 upto 20
          collect (make-instance
                    'radio-button-dialog-item
                    :cluster 1
                    :view-nick-name (+ y 200)
                    :view-position (make-point 80 y)))))

(check
  (every #'identity
         (mapcar (lambda (x)
                   (eq #$NSOffState (#/state x)))
                 (mapcar #'cocoa-ref (view-subviews (front-window))))))

(radio-button-push (view-named (+ 0 0) (front-window)))
(radio-button-push (view-named (+ 0 200) (front-window)))

(loop for view in (view-subviews (front-window))
      for nick = (view-nick-name view)
      for state = (#/state (cocoa-ref view))
      for expected-state = (if (member nick (list (+ 0 0) (+ 0 200)))
                             #$NSOnState
                             #$NSOffState)
      do (check (eq expected-state state)))
