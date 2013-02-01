; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-file-list "file-lists" "NSFMotor" "testNSFMotor.txt")

(setf *exp* (make-instance 'target-window))

(run-model)

(unintern 'check-hit)

#|
(set-view-position *exp* (make-point 150 150))
(view-position *exp*)
(easygui::view-position *exp*)
(view-size *exp*)
(#/frame (cocoa-ref (get-front-window)))
(set-view-position (get-front-window) 10 10)
(setf easygui::*screen-flipped* t)
easygui::*screen-flipped*
(easygui::view-content-rect (get-front-window))
(easygui::screen-height)
|#
