; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defmethod load-file :after (file)
  (when (search "testTutorials" file)
    (dolist (symbol (list 'print-results 'reset-display 'run-block))
      (fmakunbound symbol)))
  (when (search "testTamboDisModel" file)
    (fmakunbound 'begin-experiment))
  (when (search "testStanleyThesis" file)
    (fmakunbound 'begin-experiment)))

; Don't reload ccl-simple-view code anymore; already loaded.
(let ((*load-sv-dev-files-p* nil))
  (load-file-list "file-lists" "CI" "testCI.txt"))
