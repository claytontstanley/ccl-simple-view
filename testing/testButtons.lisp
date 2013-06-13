; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *win*
      (make-instance 'rpm-real-window
                     :view-subviews
                     (list 
                       (make-instance 'button-dialog-item
                                      :view-nick-name :tb
                                      :dialog-item-text "foo"
                                      :part-color-list (list :text (color-symbol->system-color 'red)))
                       (make-instance 'check-box-dialog-item
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

(check
  (equal '((oval oval light-gray)
           (text text red)
           (visual-object box light-gray)
           (text text orange)
           (oval oval light-gray)
           (text text black))
         (print
           (mapcar (lambda (chunk)
                     (list 
                       (chunk-slot-value-fct  chunk 'kind) 
                       (chunk-slot-value-fct  chunk 'value)
                       (chunk-slot-value-fct  chunk 'color) 
                       ))
                   (visicon-chunks (get-module :vision) t)))))
