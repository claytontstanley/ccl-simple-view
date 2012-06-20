; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-as-lst "votebox" "Virtual Cursor" "osx-virtual-cursor.lisp")
(load-as-lst "bincarbon" "misc-lib.lisp")

(load-as-lst "bincarbon" "pict-svm.lisp")

(load-as-lst "votebox" "Emulator Files" "VBEmulator.lisp")
