; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defmethod load-file :after (file)
  (when (search "testReplayExperimentWindow" file)
    (fmakunbound 'begin-experiment)))

(load-file-list "file-lists" "TouchKeyboard" "loaded.txt")
(load-file-list "file-lists" "PasswordEntryTextView" "loaded.txt")
(load-file-list "file-lists" "DMTracker" "testDMTracker.txt")
(load-file-list "file-lists" "ReplayExperimentWindow" "loaded.txt")

