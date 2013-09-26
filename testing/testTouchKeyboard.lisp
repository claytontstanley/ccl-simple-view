

(make-instance
  'window
  :view-subviews (list
                   (make-instance 'phone-touch-keyboard
                                  :view-nick-name :keyboard
                                  :view-position (make-point 0 100)
                                  :view-size (make-point 100 100)
                                  ))
  :view-size (make-point 100 200))

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

