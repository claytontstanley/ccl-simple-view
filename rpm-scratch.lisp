;(load "~/src/actr6/load-act-r-6.lisp")

;(start-environment)
;(visible-virtuals-available?)
;(stop-environment)


#+clozure (require :cocoa)
#+clozure (require :easygui)
#+clozure (load "~/src/mcl-migration/actr6/devices/ccl/uwi.lisp")
#+clozure (load "~/src/mcl-migration/actr6/devices/ccl/device.lisp")
#+clozure (load "~/src/mcl-migration/easygui/extensions.lisp")

;(print *features*)

(defparameter *win* nil)
(defparameter *view* nil)

(setf *win* (make-instance 'rpm-real-window))
(sleep .5)
(close-rpm-window *win*)


(setf *win* (make-rpm-window 
              :visible t
              :width 400
              :height 400))
(sleep .5)

(setf *view* (make-button-for-rpm-window 
               *win*
               :x 100
               :y 100
               ;:action (lambda (obj) (print "hello"))))
               :action nil)) 
(add-visual-items-to-rpm-window *win* *view*)
(sleep .5)

(setf *view* (make-static-text-for-rpm-window
               *win*
               :x 200
               :y 100
               :text "hello, world"))
(add-visual-items-to-rpm-window *win* *view*)
(sleep .5)

(setf *view* (make-line-for-rpm-window
               *win*
               (list 100 200)
               (list 200 220)
               'green))
(add-visual-items-to-rpm-window *win* *view*)

(setf *view* (make-line-for-rpm-window
               *win*
               (list 100 220)
               (list 200 200)
               'yellow))
(add-visual-items-to-rpm-window *win* *view*)

(setf *view* (make-line-for-rpm-window
               *win*
               (list 300 200)
               (list 200 220)
               'black))
(add-visual-items-to-rpm-window *win* *view*)

(setf *view* (make-line-for-rpm-window
               *win*
               (list 300 220)
               (list 200 200)
               'blue))

(add-visual-items-to-rpm-window *win* *view*)
(sleep .5)

;(inspect *view*)
;(inspect *win*)

(close-rpm-window *win*)


















