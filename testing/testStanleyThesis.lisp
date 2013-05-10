; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-file-list "file-lists" "StanleyThesis" "testStanleyThesis.txt")

(thing2lisp)

#|

(load-as-lst ".." "submodules" "stanley-thesis" "participant data" "eye-tracked participants" "source" "subj050.lisp")

(with-open-file (strm "foo.lisp" :direction :output :if-exists :supersede) 
  (write-readable *library-experiment-window* strm))

(load "~/foo.lisp")

(begin-experiment
  :eyetracking nil
  :allowing-quit t
  :short-stack 20)

(subviews *library-experiment-window*)
(show-cursor)
(make-instance 'window)
(inspect *)
(write-readable (front-window) *)
(write-readable *library-experiment-window* t)
(inspect *library-experiment-window*)
*features*
|#
