; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-file-list "file-lists" "StanleyThesis" "testStanleyThesis.txt")

; opens a window, sets contents to *library-experiment-window*, and closes window when done
(let ((*path* (format nil "~a/" (path-as-lst ".." "submodules" "stanley-thesis" "participant data" "eye-tracked participants" "source"))))
  (things2lisp :snums (list 50)))

(defmethod play-expt-with-window ((window window) &optional (constraints))
  (let ((prop (make-instance 'analysis-properties))) 
    (play-expt :prop prop
               :win window 
               :constraints constraints)
    (window-close
      (replay-win
        (replay-mod prop)))))

; *library-experiment-window* is currently set to subject 50 source lisp file
(play-expt-with-window *library-experiment-window*
                       (list 
                         (make-instance 'trial-constraint :index 7)
                         (make-instance 'trial-constraint :index 9)))

; overwrites *library-experiment-window* with subject 50 combined file; just checking that this file can be read in
; opens *library-experiment-window*
(load-as-lst ".." "submodules" "stanley-thesis" "participant data" "eye-tracked participants" "source" "subj050 combined.lisp")

; close it afterwards
(window-close *library-experiment-window*)
#|


(let ((*path* "/Volumes/claytonstanley/Desktop/")) 
  (let ((*library-experiment-window*))
    (things2lisp :snums (list 2))
    (setf *library-experiment-window-2* *library-experiment-window*)))

(let ((*library-experiment-window* *library-experiment-window-2*))
  (play-expt-with-window *library-experiment-window*))

(load-as-lst ".." "submodules" "stanley-thesis" "participant data" "eye-tracked participants" "source" "subj050.lisp")

(with-open-file (strm "foo.lisp" :direction :output :if-exists :supersede) 
  (write-readable *library-experiment-window* strm))

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
