
#+:clozure (require :ccl-simple-view)
#+:digitool (require :bootstrap-mcl)

#+:clozure
(defmacro ccl::ppc-ff-call (name &rest args)
  `(external-call ,name ,@args))

#+:clozure
(defmacro ccl::with-rectangle-arg ((var left &optional top right bottom) &body body)
  `(with-rectangle-arg (,var ,left ,top ,right ,bottom)
     ,@body))

#+:clozure
(defun %put-long (ptr data &optional (offset 0))
    (setf (%get-long (%inc-ptr ptr offset)) data)) 

; defsystem.lisp in ccl/tools overwrites default #'require function
; with a broken version. There is a global variable that
; you can set in this file to not redefine require, but #'require is
; redefined by that file at compile time, which means that the only
; way to change that variable's value is to change it in the src 
; code, which is unacceptable due to long-term maintenance issues.
; So the default defsystem.lisp is loaded, and #'require is set back
; to its default explicitly in the code below.
; 
; defsystem.lisp also messes with the #@ read macro in the readtable,
; so restoring default readtable after defsystem.lisp is loaded.

(defun load-defsystem ()
  (let ((read-table (copy-readtable)))
    (let ((orig-require #'require))
      (unwind-protect (require :defsystem)
        (with-continue
          (setf (symbol-function 'require) orig-require))
        (copy-readtable read-table *readtable*)))))

(load-defsystem)

(defparameter *path-separator*
  #+:digitool ":"
  #+:clozure "/")

(let ((dmtracker-path
        (format nil "~a~a~a" (directory-namestring *load-truename*) "DMTracker" *path-separator*)))
  (let ((*module-search-path*
          (cons (pathname dmtracker-path) *module-search-path*))) 
    (setf (logical-pathname-translations "DMTracker") `(("**;*.*" ,(pathname dmtracker-path))))
    (require :dmtracker) 
    (require :dmtracker-ff)))

(let ((mk::*tell-user-when-done* t)
      (mk::*load-source-if-no-binary* t)
      (mk::*bother-user-if-no-binary* nil)
      (mk::*load-source-instead-of-binary* t)
      (mk::*compile-during-load* t))
  (mk:load-system "DMTracker"))

(provide :DMTracker-bootstrap)

