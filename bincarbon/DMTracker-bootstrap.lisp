#+:clozure (require :ccl-simple-view)

#+:clozure
(defmacro ccl::ppc-ff-call (name &rest args)
  `(external-call ,name ,@args))

(defun load-defsystem ()
  (let ((read-table (copy-readtable)))
    (require :defsystem)
    (copy-readtable read-table *readtable*)))

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

(mk:load-system "DMTracker")

(provide :DMTracker-bootstrap)

