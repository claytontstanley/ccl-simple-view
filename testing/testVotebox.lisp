; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
#-:ccl-simple-view (load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(dolist (file (file-lines (path-as-lst "file-lists" "testVotebox.txt")))
  (load (path-as-lst file)))

(do-experiment wind)

#|
(inspect *view*)

(remove-subviews wind *view*)

(inspect wind)

(first (subviews wind))

(setf *view* *)

(add-subviews wind *view*)

(view-named :office-name wind)
(inspect *)
()
(inspect #'initialize-instance)
(fmakunbound 'easygui:set-back-color)
(inspect #'easygui:set-back-color)

(set-back-color * (color-symbol->system-color 'blue))
(set-back-color * (#/clearColor ns:ns-color))

(#/setDrawsBackground: (cocoa-ref *) #$YES)

|#
