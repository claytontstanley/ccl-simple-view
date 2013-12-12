; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defmethod load-file :after (file)
  (awhile (front-window)
    (window-close it)))

; Don't reload ccl-simple-view code anymore; already loaded.
(let ((*load-sv-dev-files-p* nil))
  (load-file-list "file-lists" "Unit" "testUnit.txt"))

