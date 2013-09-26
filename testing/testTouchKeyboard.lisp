

(make-instance
  'window
  :view-position (make-point 10 100)
  :view-subviews (list
                   (make-instance 'phone-portrait-touch-keyboard
                                  :view-nick-name :keyboard
                                  :initial-layout :sd3
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
                                  :initial-layout :sd1-shift
                                  :view-position (make-point 0 100)
                                  :view-size (make-point (* 80 11) (* 27 11))
                                  ))
  :view-size (make-point (* 80 11) (+ 100 (* 27 11))))
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

