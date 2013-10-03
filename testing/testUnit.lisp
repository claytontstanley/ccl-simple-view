; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defmethod load-file :after (file)
  (mapcar '#/close (ns-array->list (#/windows (#/sharedApplication ns:ns-application)))))

; Don't reload ccl-simple-view code anymore; already loaded.
(let ((*load-sv-dev-files-p* nil))
  (load-file-list "file-lists" "Unit" "testUnit.txt"))

