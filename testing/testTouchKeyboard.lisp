
(defclass my-touch-keyboard (touch-keyboard)
  ())

(defclass phone-portrait-touch-keyboard (my-touch-keyboard phone-portrait-touch-keyboard-mixin)
  ())

(defclass phone-landscape-touch-keyboard (my-touch-keyboard phone-landscape-touch-keyboard-mixin)
  ())

(defclass tablet-portrait-touch-keyboard (my-touch-keyboard tablet-portrait-touch-keyboard-mixin)
  ())

(defclass tablet-landscape-touch-keyboard (my-touch-keyboard tablet-landscape-touch-keyboard-mixin)
  ())

; Write an :after method to fire custom code immediately after any view in the keyboard is changed/added/removed.
; This method fires for example, after the layout changes from :sd1 to :sd2, after a popup key is displayed, after a popup key is removed, etc.
(defmethod keyboard-views-changed :after ((view my-touch-keyboard))
  (beep))

; Write an :after method to run custom code that wants to be informed when a keypress happens (e.g., a key logger class)
; layout changes are already handled in the base class
(defmethod mouse-click-handled-on-key :after ((keyboard my-touch-keyboard) (layout touch-layout) (key touch-key) (val t))
  ())
    

(make-instance
  'window
  :view-position (make-point 10 100)
  :view-subviews (list
                   (make-instance 'phone-portrait-touch-keyboard
                                  :view-nick-name :keyboard
                                  :initial-layout :sd1
                                  :view-position (make-point 0 100)
                                  :view-size (make-point (* 40 14) (* 27 14))
                                  ))
  :view-size (make-point (* 40 14) (+ 100 (* 27 14))))

(make-instance
  'window
  :view-position (make-point 10 100)
  :view-subviews (list
                   (make-instance 'phone-landscape-touch-keyboard
                                  :view-nick-name :keyboard
                                  :initial-layout :sd3
                                  :view-position (make-point 0 100)
                                  :view-size (make-point (* 80 11) (* 27 11))
                                  ))
  :view-size (make-point (* 80 11) (+ 100 (* 27 11))))

(make-instance
  'window
  :view-position (make-point 10 100)
  :view-subviews (list
                   (make-instance 'tablet-portrait-touch-keyboard
                                  :view-nick-name :keyboard
                                  :initial-layout :sd1
                                  :view-position (make-point 0 100)
                                  :view-size (make-point (* 32 19) (* 11 19))
                                  ))
  :view-size (make-point (* 32 19) (+ 100 (* 11 19))))


(make-instance
  'window
  :view-position (make-point 10 100)
  :view-subviews (list
                   (make-instance 'tablet-landscape-touch-keyboard
                                  :view-nick-name :keyboard
                                  :initial-layout :sd1
                                  :view-position (make-point 0 100)
                                  :view-size (make-point (* 32 19) (* 11 19))
                                  ))
  :view-size (make-point (* 32 19) (+ 100 (* 11 19))))
#|
(remove-keys (view-named :keyboard (front-window)))
(print-pool)
(get-resource-val "smartphone-portrait-sd1" 'image)
(get-resource-val "voteboxbg" 'image)
(setf (pict-id (view-named :keyboard (front-window))) "smartphone-portrait-sd1")
(getf (layouts (view-named :keyboard (front-window))) :smartphone-portrait-sd1)
(get-layout (view-named :keyboard (front-window)) :smartphone-portrait-sd1)
(subviews (view-named :keyboard (front-window)))
(#/image (cocoa-ref *))
(invalidate-view (front-window))
(unintern 'keys)
(inspect *)
(inspect (front-window))
(setf *resource-pool* (init-pool))
(print-pool)
(alloc-resources)
(open-resource-folder (format nil "~a~a/" *dir* "touch-keyboard-images"))
(print *dir*)
|#



#|
(remove-keys (view-named :keyboard (front-window)))
(print-pool)
(get-resource-val "smartphone-portrait-sd1" 'image)
(get-resource-val "voteboxbg" 'image)
(setf (pict-id (view-named :keyboard (front-window))) "smartphone-portrait-sd1")
(getf (layouts (view-named :keyboard (front-window))) :smartphone-portrait-sd1)
(get-layout (view-named :keyboard (front-window)) :smartphone-portrait-sd1)
(subviews (view-named :keyboard (front-window)))
(#/image (cocoa-ref *))
(invalidate-view (front-window))
(unintern 'keys)
(inspect *)
(inspect (front-window))
(setf *resource-pool* (init-pool))
(print-pool)
(alloc-resources)
(open-resource-folder (format nil "~a~a/" *dir* "touch-keyboard-images"))
(print *dir*)
|#

#|
(remove-keys (view-named :keyboard (front-window)))
(print-pool)
(get-resource-val "smartphone-portrait-sd1" 'image)
(get-resource-val "voteboxbg" 'image)
(setf (pict-id (view-named :keyboard (front-window))) "smartphone-portrait-sd1")
(getf (layouts (view-named :keyboard (front-window))) :smartphone-portrait-sd1)
(get-layout (view-named :keyboard (front-window)) :smartphone-portrait-sd1)
(subviews (view-named :keyboard (front-window)))
(#/image (cocoa-ref *))
(invalidate-view (front-window))
(unintern 'keys)
(inspect *)
(inspect (front-window))
(setf *resource-pool* (init-pool))
(print-pool)
(alloc-resources)
(open-resource-folder (format nil "~a~a/" *dir* "touch-keyboard-images"))
(print *dir*)
|#

