; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(setf *win*
      (make-instance
        'window
        :view-position (make-point 10 10)
        :view-subviews
        (list
          (make-dialog-item 'static-text-dialog-item
                            (make-point 100 100)
                            (make-point 50 50)
                            "bar")
          (make-dialog-item 'sequence-dialog-item
                            (make-point 20 20)
                            (make-point 100 100)
                            "foo" 
                            (lambda (item) (beep))
                            :view-font *fred-default-font-spec*
                            :table-sequence (list "}}}" "--" "*")
                            :cell-size (make-point 100 20)
                            :view-nick-name :sdi
                            ))))

(print (select-item-from-list (list 'a 'b "c" "d" "hello" (list 4 3 2))))

#|
(invalidate-view *win*))
(setf *sv-log-level* 0)
(dialog-item-text *view*)
(print (cocoa-ref *view*))
(#/stringValue (cocoa-ref *view*))
(selected-cells *view*)
(#/title
 (index-to-cell *view* 2))
(#/setIntercellSpacing: (cocoa-ref *view*) (ns:make-ns-size 50 50))
(#/setCellSize: (cocoa-ref *view*) (ns:make-ns-size 20 20))
(setf *win* (make-instance 'window))
(add-subviews *win* *view*)
(#/mode (cocoa-ref *view*))
(#/cellSize (cocoa-ref *view*))
(set-table-sequence *view* (list "alpha" "..." "--" "*"))
(invalidate-view *view*)
(#/autorecalculatesCellSize (cocoa-ref *view*))
(#/autosizesCells (cocoa-ref *view*))
(#/setAutosizesCells: (cocoa-ref *view*) #$YES)
(#/sizeToCells (cocoa-ref *view*))
|#
