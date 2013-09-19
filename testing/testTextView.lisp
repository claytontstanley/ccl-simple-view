; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-as-lst ".." "bincarbon" "direct-event-calls.lisp")

(make-instance
  'window
  :view-subviews
  (list
    (make-instance
      'editable-text-dialog-item
      :view-nick-name :foo
      :view-font '("Courier" 11)
      :dialog-item-text "abc"
      :view-size (make-point 100 50))))

(event-dispatch)
(left-mouse-click (view-position (front-window)))
(keypress #\tab)

(let ((font (view-font (view-named :foo (front-window)))))
  (check (string-equal (font-name font) "Courier"))
  (check (= (font-point font) 11)))

(sleep .5)
(progn
  (keypress #\a)
  (keypress #\return)
  (keypress-on-view (view-named :foo (front-window)) (format nil "~%"))
  (keypress-on-view (view-named :foo (front-window)) #\newline)
  (keypress #\b)
  )

(check (string-equal (dialog-item-text (view-named :foo (front-window)))
                     (format nil "abca~%~%~%b")))
