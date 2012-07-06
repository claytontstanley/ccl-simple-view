; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
#-:ccl-simple-view (load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

;(setf easygui::*debug-cocoa-calls* t)

(load-in-bincarbon "misc-lib.lisp" "timer.lisp" "snd-player.lisp" "pict-svm.lisp" "procedure-window2.lisp" "base-trek-tasks.lisp")
;(provide :timer)
;(load-in-bincarbon "snd-player.lisp" "pict-svm.lisp" "misc-lib.lisp" "procedure-window2.lisp" "base-trek-tasks.lisp")

;(load-in-bincarbon "CFBundle.lisp" "seq-math.lisp" "launch-url.lisp" "experiment-window4.lisp")
(provide :cfbundle)
(load-in-bincarbon "seq-math.lisp" "launch-url.lisp" "experiment-window4.lisp")

(load-in-bincarbon "blinker.lisp" "menubar-hide.lisp")

(load-as-lst "phaser" "X83.lisp")

;(get-string-from-user "hello")
;
;(setf *ret* (list 5 4))
;
#|
(make-instance 'main-control-window)
(setf *exp* *)
(train-p *exp*)
(inspect *exp*)
(setup-experiment *exp*)
(run-experiment *exp*)
(run-block *exp* (first (block-lst *exp*)))
(all-processes)

(task-window *experiment*)
(foo)

(easygui::y-or-n-dialog "help")

(multiple-value-bind (a b c) (modal-dialog (task-window *experiment*))
  ())

(feedback *experiment* "kakak")

(current-btn *experiment*)

(do-button *experiment* (current-btn *experiment*))
(current-trial *experiment*)
(print *ret*)

(setf *ret* (list 1 1 1 1))

(xcond *experiment*)

(setf *ret* (list 1 2 3))

|#
