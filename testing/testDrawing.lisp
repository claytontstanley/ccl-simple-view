(setf *sv* (make-instance 'simple-view
                          :view-size #@(100 100)
                          :fore-color *blue-color*))

(setf *win* (make-instance 'window 
                           :view-subviews (list *sv*)
                           :fore-color *green-color*))


(get-fore-color *win*)

(defmethod initialize-instance :after ((win window) &key)
  (set-fore-color win (get-fore-color win)))

(defmethod set-fore-color ((win window) new-color)
  (unwind-protect (easygui:get-fore-color win)
    (#/set new-color)))

(#/set *red-color*)

(#/currentContext ns:ns-graphics-context)

(defmethod view-draw-contents ((view (eql *sv*)))
  ;(with-focused-view view
  ;  (with-fore-color *blue-color*
      ;(#/set *red-color*)
      (paint-rect #@(0 0) (view-size view))
      )
  ;  )
  ;)

(invalidate-view (first (subviews *win*)))

(set-fore-color *win* *red-color*)

(setf easygui::*suppress-window-flushing* t)

(#/flushWindow (cocoa-ref *win*))
