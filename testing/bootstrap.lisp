(defparameter *path-separator*
  #+:digitool ":"
  #+:clozure "/")

(defparameter *base-repo-namestring*
  (format nil "~a~a~a" (directory-namestring *load-truename*) ".." *path-separator*))

(load (format nil "~a~a" (directory-namestring *load-truename*) "utilities.lisp"))

#-:act-r-6.0 (load-as-lst ".." "submodules" "actr6" "load-act-r-6.lisp")

#+clozure
(cond ((member "swank-repl" *modules* :test #'string-equal)
       (load-file-list ".." "build" "file-list.txt"))
      (t
       (load-as-lst ".." "build" "ccl-simple-view.lisp")))

#+digitool (load-as-lst "bootstrap-mcl.lisp")

