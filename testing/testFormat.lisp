(setf *sv*
      (make-instance 'simple-view
                     :view-size (make-point 100 100)
                     :view-position (make-point 10 10)
                     :view-nick-name :sv))

(setf *win*
      (make-instance 'window
                     :fore-color *blue-color*
                     :view-position (make-point 10 600)
                     :view-subviews 
                     (list *sv*)))

(defmethod view-draw-contents ((view (eql *sv*)))
  (move-to *sv* 10 10)
  (with-fore-color *red-color*
    (format *sv* "pello"))
  (move-to *sv* 10 30)
  (with-font-focused-view view
    (#_drawstring (objc:make-nsstring "yello")))
  )


#|
(invalidate-view *win*)

(set-fore-color *win* *blue-color*)
(set-fore-color *win* *green-color*)

|#
