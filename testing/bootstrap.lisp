(load (format nil "~a~a" (directory-namestring *load-truename*) "utilities.lisp"))

(defparameter *base-repo-namestring*
  (format nil "~a~a~a" (directory-namestring *load-truename*) ".." *path-separator*))

(defun load-file-list (&rest file-list)
  "Loads each file in file-list, where the location of 
   file-list is a relative path from the load file's directory
   and the location of each file in file-list is a relative path from
   the base repo's directory"
  (dolist (file (file-lines (apply #'path-as-lst file-list)))
    (let ((file (replace-all file "/" *path-separator*)))
      (let ((file (format nil "~a~a" *base-repo-namestring* file)))
        (cond ((search "load-act-r-6.lisp" file)
               #-:act-r-6.0 (load file))
              (t
               (load file)))))))

#-:act-r-6.0 (load-as-lst ".." "submodules" "actr6" "load-act-r-6.lisp")

#+:clozure
(cond ((member "swank-repl" *modules* :test #'string-equal)
       (load-file-list ".." "build" "file-list.txt"))
      (t
       (load-as-lst ".." "build" "ccl-simple-view.lisp")))

(load-file-list "file-lists" "all.txt")

#+:digitool (load-as-lst "bootstrap-mcl.lisp")

