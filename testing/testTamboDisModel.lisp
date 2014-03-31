; Bootstrap all needed packages (loads ACT-R, Cocoa framework, etc.)
(load (format nil "~a~a" (directory-namestring *load-truename*) "bootstrap.lisp"))

(load-file-list "file-lists" "TamboDisModel" "testPhaser.txt")

(with-shadow (reset (lambda ()
                      (funcall fun-orig)
                      ; Set :static-default to nil, otherwise retrieval error and model hangs; should work though
                      ; Dan is probably still working through the issues with the new static default chunking mechanism
                      ; cts: 2014-03-31
                      (ssp :static-default nil)))
  (run-all-models :rt nil) 
  ;(run-a-model 0 :rt nil) 
  )

(setf *experiment* nil)

