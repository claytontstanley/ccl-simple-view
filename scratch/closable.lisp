
(make-instance 'window :closable-p nil)
(#/setEnabled:
 (#/standardWindowButton: (cocoa-ref (front-window))
  #$NSWindowCloseButton)
 #$YES)
(window-close *)
***
(#/close (cocoa-ref *))
#$NSWindowCloseButton
