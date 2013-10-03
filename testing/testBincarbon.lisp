; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-as-lst ".." "bincarbon" "touch-keyboard.lisp")
(load-as-lst ".." "bincarbon" "testing" "testTouchKeyboard.lisp")

(load-as-lst ".." "bincarbon" "chil-ccl-utilities.lisp")
(load-as-lst ".." "bincarbon" "direct-event-calls.lisp")
(load-as-lst ".." "bincarbon" "password-entry-text-view.lisp")
(load-as-lst ".." "bincarbon" "testing" "testPasswordEntryTextView.lisp")

(load-file-list "file-lists" "DMTracker" "testDMTracker.txt")

(load-file-list "file-lists" "ReplayExperimentWindow" "loaded.txt")

