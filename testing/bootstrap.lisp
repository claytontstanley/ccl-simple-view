
(load (format nil "~a~a" (directory-namestring *load-truename*) "utilities.lisp"))

#-:act-r-6.0 (load-as-lst "actr6" "load-act-r-6.lisp")

#+clozure (dolist (file (file-lines "~/src/mcl-migration/build/file-list.txt"))
            (unless (string-equal file "utilities.lisp")
              (load (format nil "~a/~a" "~/src/mcl-migration" file))))

#+digitool (load-as-lst "bootstrap-mcl.lisp")

