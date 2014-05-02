; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(check (eq (system-color->symbol (color-symbol->system-color 'gray))
           'gray))
(check (eq (system-color->symbol (color-symbol->system-color 'grey))
           'gray))
