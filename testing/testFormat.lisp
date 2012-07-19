(setf *sv*
      (make-instance 'simple-view
                     :view-size (make-point 100 100)
                     :view-position (make-point 10 10)
                     :view-nick-name :sv))

(setf *win*
      (make-instance 'window
                     :view-position (make-point 10 600)
                     :view-subviews 
                     (list *sv*)))
(defmethod view-draw-contents ((view (eql *sv*)))
  (move-to *sv* 10 10)
  (with-fore-color *red-color*
    (format *sv* "pello")))


#|
(invalidate-view *win*)

|#
