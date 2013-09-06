; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-as-lst ".." "bincarbon" "text-view.lisp")

(make-instance
  'window
  :view-subviews
  (list
    (make-instance
      'text-view
      :view-nick-name :foo
      :view-size (make-point 100 50))))

(left-mouse-click (view-position (front-window)))
(left-mouse-click (add-points
                    (view-position (front-window))
                    (view-center (view-named :foo (front-window)))))

(sleep .5)
(progn
  (keypress #\a)
  (keypress #\return)
  (keypress-on-view (view-named :foo (front-window)) (format nil "~%"))
  (keypress-on-view (view-named :foo (front-window)) #\newline)
  (keypress #\b)
  )

(check (string-equal (dialog-item-text (view-named :foo (front-window)))
                     (format nil "a~%~%~%b")))
