
#|
(inspect *win*)
(nth 5 (subviews *win*))
(inspect *)
*win*
(set-back-color * (color-symbol->system-color 'orange))
(#/drawsBackground (cocoa-ref *))
(#/isOpaque (cocoa-ref *))
(#/setBackgroundColor: (cocoa-ref *) (color-symbol->system-color 'green))
(#/setBackgroundColor: (cocoa-ref *) (#/clearColor ns:ns-color))
(#/setOpaque: (cocoa-ref *) #$YES)
(easygui::set-needs-display *win* t)
(#/drawsBackground (cocoa-ref *))
(#/setDrawsBackground: (cocoa-ref *) #$NO)
|#
