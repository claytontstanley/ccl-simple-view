; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(check (eq (system-color->symbol (color-symbol->system-color 'gray))
           'gray))
(check (eq (system-color->symbol (color-symbol->system-color 'grey))
           'gray))
(check (eq (system-color->symbol (color-symbol->system-color 'blue))
           'blue))
(check (null (system-color->symbol (make-color 1 1 1))))
