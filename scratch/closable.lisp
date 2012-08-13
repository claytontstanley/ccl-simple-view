
(make-instance 'window :closable-p nil)
(#/setEnabled:
 (#/standardWindowButton: (cocoa-ref (get-front-window))
  #$NSWindowCloseButton)
 #$YES)
(window-close *)
***
(#/close (cocoa-ref *))
#$NSWindowCloseButton
