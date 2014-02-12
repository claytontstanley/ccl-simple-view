; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *win* (make-instance
              'window
              :view-subviews
              (list (make-instance
                      'editable-text-dialog-item
                      :view-nick-name :et
                      :dialog-item-text "so"))))
(check (equal (mapcar #'floor (as-list (view-size (view-named :et *win*))))
              (mapcar #'floor (as-list (add-points (make-point 5 5) (view-size (content-view (view-named :et *win*))))))))
(check (equal
         (list 2 2)
         (multiple-value-list (selection-range (view-named :et *win*)))))

(check (null (#/hasVerticalScroller (cocoa-ref (view-named :et *win*)))))
(check (null (v-scroll-p (view-named :et *win*))))

(add-subviews *win*
              (make-instance
                'editable-text-dialog-item
                :dialog-item-text "bar"
                :view-font (list "Courier" 55)
                :view-nick-name :et2
                :view-position (make-point 40 40)))

(add-subviews *win*
              (make-instance
                'editable-text-dialog-item
                :dialog-item-text "foo"
                :view-nick-name :et3
                :view-size (make-point 15 15)
                :view-position (make-point 150 150)))
(check (equal
         (list 2 2)
         (multiple-value-list (selection-range (view-named :et *win*)))))
