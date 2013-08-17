; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(defun run-in-file (file fun &key (close-window-p t))
  (let ((file (if (listp file)
                file
                (list file))))
    (apply #'load-as-lst file)
    (funcall fun)
    (when close-window-p
      (close-exp-window))))

(defun do-tutorial (tutorial)
  (let ((path-to-loader
          (path-as-lst ".." "submodules" "actr6" "tutorial" tutorial "loader.lisp")))
    (print path-to-loader)
    (load path-to-loader)))

(defun do-agi-example (file)
  (run-in-file (list ".." "submodules" "actr6" "examples" "agi" file)
               (lambda ()
                 (funcall #'run-it t))
               :close-window-p nil)
  (clear-all))

(defmacro with-faster-than-real-time-run (&body body)
  `(with-shadow (run (lambda (run-time &key (real-time))
                        (declare (ignore real-time))
                        (funcall fun-orig run-time :real-time nil)))
     ,@body))

(defmacro with-visible-window-run (&body body)
  `(with-shadow (open-exp-window (lambda (title &rest args)
                                   (remf args :visible) 
                                   (apply fun-orig title :visible t args)))
     ,@body))

(defmacro with-verbose-run (&body body)
  `(with-shadow (reset (lambda ()
                         (funcall fun-orig)
                         (sgp :v t)))
     ,@body))

(do-agi-example "multiple-models-single-window.lisp")
(do-agi-example "multiple-models-multiple-windows.lisp")
(do-agi-example "single-model-multiple-windows.lisp")

; This suite takes about 4 mins to run on CCL b/c of unit4:
;(mapc #'do-tutorial (list "unit1" "unit2" "unit3" "unit4"))

; This suite takes about 4 mins to run on CCL b/c of unit7:
;(mapc #'do-tutorial (list "unit7"))

(mapc #'do-tutorial (list  "unit2" "unit3" "unit5" "unit6" "unit8"))


