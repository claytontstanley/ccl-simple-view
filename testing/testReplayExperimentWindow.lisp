; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-file-list "file-lists" "ReplayExperimentWindow" "loaded.txt")


(replay-experiment
  (load-eyetracking-experiment-window
    :dat-file (path-as-lst ".." "submodules" "DMTracker" "ccl-dmtracker-data" "subj006-track.dat")
    :lisp-file (path-as-lst ".." "submodules" "DMTracker" "ccl-dmtracker-data" "subj006.lisp")))
