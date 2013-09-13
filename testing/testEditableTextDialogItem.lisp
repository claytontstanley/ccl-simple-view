; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *win* (make-instance
              'window
              :view-subviews
              (list (make-instance
                      'editable-text-dialog-item
                      :view-nick-name :et
                      :dialog-item-text "so"))))

(check (equal
         (list 0 2)
         (multiple-value-list (selection-range (view-named :et *win*)))))

(add-subviews *win*
              (make-instance
                'editable-text-dialog-item
                :dialog-item-text "bar"
                :view-font (list "Courier" 55)
                :view-position (make-point 40 40)))

(check (equal
         (list 0 2)
         (multiple-value-list (selection-range (view-named :et *win*)))))
