; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-file-list "file-lists" "DMTracker" "testDMTracker.txt")

#|
(require :tracker-experiment-window)
(progn
  #@(5 4)
  )
(prog1 5)
(full-pathname "dmtracker:Tracker Source")
*module-search-path*
(require :defsystem)
;(MAKE-PATHNAME :HOST :UNSPECIFIC :DEVICE NIL :DIRECTORY '(:RELATIVE :DMTracker) :NAME "dmtracker" :TYPE "system" :VERSION NIL :DEFAULTS NIL :CASE NIL)
*features*
*modules*
(require "Quickdraw")
*features*
(mk:find-system :dmtracker) 
(setf (logical-pathname-translations "DMTracker")
      "foo")

(setf *modules* (remove-if (lambda (x) (string-equal x "DMTRACKER-FF")) *modules*))
(full-pathname "dmtracker/Tracker Source")
(full-pathname "dmtracker:Tracker Source")
(inspect 'require)
(full-pathname "dmtracker")
*features*
*dont-redefine-require*
(in-package "LISP")
mk::*dont-redefine-require*
(defvar *dont-redefine-require* t)
(setf *dont-redefine-require* t) 
(pushnew :openmcl *features*)
(setf (logical-pathname-translations "DMtracker")
      '(("**;*.*" "Public:Projects:DMTracker:**:*.*")))
|#
