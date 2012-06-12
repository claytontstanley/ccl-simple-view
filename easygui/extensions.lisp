(in-package :easygui)

; I think I found a bug in these two methods in the easygui package, so redefining them here with correct setNeedsDisplay: call
(defmethod (setf view-position) (point (self view))
  (running-on-main-thread ()
                          (setf (slot-value self 'position) point)
                          (when (slot-value self 'frame-inited-p)
                            (dcc (#/setFrame: (cocoa-ref self) (view-content-rect self)))
                            (dcc (#/setNeedsDisplay: (cocoa-ref self) t)))))

(defmethod (setf view-size) (point (self view))
  (running-on-main-thread ()
                          (setf (slot-value self 'size) point)
                          (when (slot-value self 'frame-inited-p)
                            (dcc (#/setFrame: (cocoa-ref self) (view-content-rect self)))
                            (dcc (#/setNeedsDisplay: (cocoa-ref self) t)))))

; easygui by default starts position 0,0 at bottom left, going to the right and up for positive values
; ; This flips the screen vertically, so that it matches MCL's default. That is, position 0,0 is at top left
(setf *screen-flipped* t)

