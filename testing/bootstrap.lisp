(defparameter *path-separator*
  #+:digitool ":"
  #+:clozure "/")

(defun load-as-lst (&rest lst)
  (let ((path
          (format nil "~{~a~}"
                  (cons (directory-namestring *load-truename*)
                        (sandwich *path-separator* lst)))))
    (print path)
    (load path)))

(defun sandwich (item lst)
  "places item in between all elements of lst, but not to the left or right of lst"
  (assert (listp lst))
  (cond ((null lst) nil)
        ((eq (length lst) 1) lst)
        (t (cons (car lst) 
                 (cons item (sandwich item (cdr lst)))))))

#-:act-r-6.0 (load-as-lst "actr6" "load-act-r-6.lisp")
#+clozure (require :cocoa)
#+clozure (require :easygui)
#+clozure (load "~/src/mcl-migration/easygui/extensions.lisp")
#+clozure (load "~/src/mcl-migration/actr6/devices/ccl/share.lisp")
#+clozure (load "~/src/mcl-migration/actr6/devices/ccl/device.lisp")
#+clozure (load "~/src/mcl-migration/actr6/devices/ccl/uwi.lisp")

#+digitool (load-as-lst "bootstrap-mcl.lisp")
