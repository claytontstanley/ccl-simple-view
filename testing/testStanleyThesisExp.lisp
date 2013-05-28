; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-file-list "file-lists" "StanleyThesisExp" "testStanleyThesis.txt")

#|
(begin-experiment
  ;:snum 1
  :eyetracking t
  :allowing-quit t
  ;:short-stack 20
  )
|#

#|
*modules*
(require :chil-utilities)
(warning "foo")
(internal-real-time->ms) 
(make-instance 'window :view-position (make-point 1900 0))
(setf *foo* (make-instance 'xfind-window-real-hum-eyetracking))
(tracker-setup (eyetracker *foo*) :head)
(find-bsd-serial-ports)
(inspect (tracker-get-datum (eyetracker *foo*) nil))
(setf *s* (tracker-data-source (eyetracker *foo*)))
(setf *mouse-down-p* nil)
*mouse-down-p*
(progn
  (stream-clear-input *s*)
  (sleep .011)
  (clear-through-latest-delimiter *s*))
(stream-listen *s*)
(dotimes (i 100)
  (print 
    (stream-tyi *s*)))
(calibrate-eyetracker *foo*)
|#
