; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-in-bincarbon "misc-lib.lisp" "timer.lisp" "snd-player.lisp" "pict-svm.lisp" "procedure-window2.lisp" "base-trek-tasks.lisp")
;(provide :timer)
;(load-in-bincarbon "snd-player.lisp" "pict-svm.lisp" "misc-lib.lisp" "procedure-window2.lisp" "base-trek-tasks.lisp")

;(load-in-bincarbon "CFBundle.lisp" "seq-math.lisp" "launch-url.lisp" "experiment-window4.lisp")
(provide :cfbundle)
(load-in-bincarbon "seq-math.lisp" "launch-url.lisp" "experiment-window4.lisp")

(load-in-bincarbon "blinker.lisp" "menubar-hide.lisp")

(load-as-lst "phaser" "X83.lisp")

(get-string-from-user "hello")
