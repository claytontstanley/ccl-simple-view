(setf *win*
      (make-instance
        'window
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

#|
(invalidate-view *win*))
(defun make-dialog-item (class position size text &optional action &rest attributes)
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
  (set-table-sequence *view* (list "kdkd" "..." "--" "*"))
  (invalidate-view *view*)
  (#/autorecalculatesCellSize (cocoa-ref *view*))
  (#/autosizesCells (cocoa-ref *view*))
  (#/setAutosizesCells: (cocoa-ref *view*) #$YES)
  (#/sizeToCells (cocoa-ref *view*))
  |#
