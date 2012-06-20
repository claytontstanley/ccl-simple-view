; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-as-lst "bincarbon" "timer.lisp")
(load-as-lst "bincarbon" "snd-player.lisp")
(load-as-lst "bincarbon" "procedure-window2.lisp")
(load-as-lst "bincarbon" "base-trek-tasks.lisp")

(load-as-lst "bincarbon" "CFBundle.lisp")
(load-as-lst "bincarbon" "seq-math.lisp")
(load-as-lst "bincarbon" "launch-url.lisp")
(load-as-lst "bincarbon" "experiment-window4.lisp")

(load-as-lst "bincarbon" "blinker.lisp")
(load-as-lst "bincarbon" "menubar-hide.lisp")
(load-as-lst "phaser" "X83.lisp")

