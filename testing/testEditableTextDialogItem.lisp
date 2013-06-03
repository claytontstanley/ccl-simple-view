(setf *win* (make-instance
              'window
              :view-subviews
              (list (make-instance
                      'editable-text-dialog-item
                      :dialog-item-text "fo"))))

(check (equal
         (list 0 2)
         (multiple-value-list (selection-range *win*))))

(add-subviews *win*
              (make-instance
                'editable-text-dialog-item
                :dialog-item-text "bar"
                :view-position (make-point 40 40)))

(check (equal
         (list 0 2)
         (multiple-value-list (selection-range *win*))))
