; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))


(load-file-list "file-lists" "Phaser" "testPhaser.txt")

(begin-experiment)

;(get-string-from-user "hello")
;
;(setf *modal-dialog-ret* (list 5 4))
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

(feedback *experiment* "kayak")

(current-btn *experiment*)

(do-button *experiment* (current-btn *experiment*))
(current-trial *experiment*)
(print *modal-dialog-ret*)

(setf *modal-dialog-ret* (list 1 1 1 1))

(xcond *experiment*)

(setf *modal-dialog-ret* (list 1 2 3))

|#
