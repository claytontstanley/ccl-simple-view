; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

; Don't reload ccl-simple-view code anymore; already loaded.
(let ((*load-sv-dev-files-p* nil))
  (load-file-list "file-lists" "CI" "testCI.txt"))
