; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-as-lst ".." "bincarbon" "chil-ccl-utilities.lisp")
(load-as-lst ".." "bincarbon" "direct-event-calls.lisp")
(load-as-lst ".." "bincarbon" "password-entry-text-view.lisp")

(setf *win*
      (make-instance
        'window
        :view-subviews
        (list
          (make-instance
            'password-entry-text-view
            :view-size (make-point 100 30)
            
            :view-position (make-point 0 10)
            :view-nick-name :pw
            :view-font (list "Courier" 6)
            :text "hello, world"
            )
          (make-instance
            'password-entry-text-view
            :view-size (make-point 100 30)
            :visible-char-time-secs .1 
            :view-position (make-point 0 50))
          (make-instance
            'password-entry-text-view
            :view-size (make-point 100 30)
            :last-char-can-show-p nil
            :view-nick-name :pw-not-vis
            :view-position (make-point 0 100))
          )))

(let ((view (view-named :pw *win*)))
  (set-dialog-item-text view  "") 
  (dotimes (i 5)
    (keypress-on-view view "ab")
    (sleep .1))
  (check (string-equal (dialog-item-hidden-text (view-named :pw *win*)) "*********b"))
  (dotimes (i 5)
    (keypress-on-view view  #\rubout))
  (check (string-equal (dialog-item-hidden-text (view-named :pw *win*)) "*****"))
  (keypress-on-view view "abc")
  (dialog-item-hidden-text view ))

(let ((view (view-named :pw-not-vis *win*)))
  (keypress-on-view view  "a")
  (check (string-equal (dialog-item-hidden-text view ) "*")))
