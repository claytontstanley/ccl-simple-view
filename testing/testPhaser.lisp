; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

;(load-in-bincarbon "timer.lisp" "snd-player.lisp" "procedure-window2.lisp" "base-trek-tasks.lisp")
(provide :timer)
(provide :snd-player)
(load-in-bincarbon "pict-svm.lisp" "misc-lib.lisp" "procedure-window2.lisp" "base-trek-tasks.lisp")

;(load-in-bincarbon "CFBundle.lisp" "seq-math.lisp" "launch-url.lisp" "experiment-window4.lisp")
(provide :cfbundle)
(load-in-bincarbon "seq-math.lisp" "launch-url.lisp" "experiment-window4.lisp")

(load-in-bincarbon "blinker.lisp" "menubar-hide.lisp")

(load-as-lst "phaser" "X83.lisp")

