(setf *win*
      (make-instance 'rpm-real-window
                     :view-subviews
                     (list 
                       (make-instance 'button-dialog-item
                                      :view-nick-name :tb
                                      :dialog-item-text "foo"
                                      :part-color-list (list :text (color-symbol->system-color 'red)))
                       (make-instance 'button-dialog-item
                                      :view-nick-name :cb
                                      :part-color-list (list :text (color-symbol->system-color 'green))
                                      :dialog-item-text "hello"
                                      :view-position (make-point 30 30)))))
(add-visual-items-to-rpm-window
  *win*
  (make-button-for-rpm-window
    *win*
    :x 100
    :y 100
    :height 50
    :width 50))

(sleep 2)
(set-part-color
  (view-named :cb *win*)
  :text (color-symbol->system-color 'orange))

(clear-all)
(define-model foo
  ())
(install-device *win*)

(proc-display)
(print-visicon)
