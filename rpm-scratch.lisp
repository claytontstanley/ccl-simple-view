
;(load "~/src/actr6/load-act-r-6.lisp")

(setf *win* (make-instance 'rpm-real-window))

(sleep .5)

(close-rpm-window *win*)

;(setf *win* (make-rpm-window :visible t))

(sleep .5)

(close-rpm-window *win*)
