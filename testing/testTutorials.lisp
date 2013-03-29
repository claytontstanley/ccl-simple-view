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

(do-agi-example "multiple-models-single-window.lisp")
(do-agi-example "multiple-models-multiple-windows.lisp")
(do-agi-example "single-model-multiple-windows.lisp")

; This suite takes about 4 mins to run on CCL b/c of unit4:
;(mapc #'do-tutorial (list "unit1" "unit2" "unit3" "unit4"))

; This suite takes about 4 mins to run on CCL b/c of unit7:
;(mapc #'do-tutorial (list "unit7"))

(mapc #'do-tutorial (list  "unit2" "unit3" "unit5" "unit6" "unit8"))

; These 3 functions are defined only in the tutorials (not part of main act-r functions), and they 
; collide with functions defined in chil library utilities, so once done with running tutorials, be nice and clean
; up after ourselves, so that collisions don't occur if you run other tests after this one

(fmakunbound 'reset-display) 
(fmakunbound 'run-block)
(fmakunbound 'print-results)

