#+:clozure (require :ccl-simple-view)

#+:clozure
(defmacro ccl::ppc-ff-call (name &rest args)
  `(external-call ,name ,@args))

(defun load-defsystem ()
  (let ((read-table (copy-readtable)))
    (let ((orig-require #'require))
      (unwind-protect (require :defsystem)
        (with-continue
          (setf (symbol-function 'require) orig-require))
        (copy-readtable read-table *readtable*)))))

#+:clozure (load-defsystem)

#+:clozure
(progn
  (provide :cfbundle)
  (provide "cfbundle"))

(let ((dmtracker-path
        (format nil "~a/" (path-as-lst ".." "submodules" "DMTracker"))))
  (let ((*module-search-path*
          (cons (pathname dmtracker-path) *module-search-path*))) 
    (setf (logical-pathname-translations "DMTracker") `(("**;*.*" ,(pathname dmtracker-path))))
    (require :dmtracker.system) 
    (require :dmtracker-ff.system)))

(let ((mk:*dont-redefine-require* t)
      (mk::*tell-user-when-done* t)
      (mk::*load-source-if-no-binary* t)
      (mk::*bother-user-if-no-binary* nil)
      (mk::*load-source-instead-of-binary* t)
      (mk::*compile-during-load* t))
  (mk:load-system "DMTracker"))

(provide :DMTracker-bootstrap)

