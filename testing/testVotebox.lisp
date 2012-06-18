; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-as-lst "votebox" "Virtual Cursor" "osx-virtual-cursor.lisp")
(load-as-lst "votebox" "RMCL" "bincarbon" "misc-lib.lisp")

(load-as-lst "votebox" "RMCL" "bincarbon" "pict-svm.lisp")

(load-as-lst "votebox" "Emulator Files" "VBEmulator.lisp")



;ls "votebox/RMCL/bincarbon"


