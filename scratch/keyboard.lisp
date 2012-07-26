
(defun press-key (win key)
  (let ((key (objc:make-nsstring (format nil "~a" key))))
    (let ((the-event 
            (#/keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode: ns:ns-event
             #$NSKeyDown
             (ns:make-ns-point 0 0)
             0
             (#/timeIntervalSinceReferenceDate ns:ns-date)
             (#/windowNumber (cocoa-ref win))
             (#/graphicsContext (cocoa-ref win))
             key
             key
             #$NO
             0)))
      (#/keyDown: (cocoa-ref win) the-event))))
