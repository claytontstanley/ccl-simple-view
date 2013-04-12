#+:clozure (setf ccl:*default-external-format* :utf-8)
#+:clozure (pushnew :sv-dev *features*)
#+:clozure (defvar *load-sv-dev-files-p* t)

(load (format nil "~a~a" (directory-namestring *load-truename*) "utilities.lisp"))

(defun load-file-list (&rest file-list)
  "Loads each file in file-list, where the location of 
   file-list is a relative path from the load file's directory
   and the location of each file in file-list is a relative path from
   the base repo's directory"
  (let ((base-repo-namestring
          (format nil "~a~a~a" (directory-namestring *load-truename*) ".." *path-separator*)))
    (dolist (file (file-lines (apply #'path-as-lst file-list)))
      (let ((file (replace-all file "/" *path-separator*)))
        (let ((file (format nil "~a~a" base-repo-namestring file)))
          (cond ((search "load-act-r-6.lisp" file)
                 #-:act-r-6.0 (load file))
                (t
                 (load file))))))))

#-:act-r-6.0 (load-as-lst ".." "submodules" "actr6" "load-act-r-6.lisp")

#+:clozure
(cond ((and (member "swank-repl" *modules* :test #'string-equal)
            *load-sv-dev-files-p*)
       (load-file-list ".." "build" "file-list.txt")
       (load-file-list ".." "build" "file-list-device.txt")
       (load-file-list ".." "build" "file-list-uwi.txt"))
      (t nil))

#+:clozure (setf *resource-pool* (init-pool))

(load-as-lst ".." "submodules" "lisp-dev" "Lisp-Unit-Testing-Framework" "unitTestFramework.lisp")

#+:digitool (load-as-lst ".." "bincarbon" "bootstrap-mcl.lisp")

