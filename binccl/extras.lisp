(defun save-snapshot (fname)
  "Saves snapshot of current window as a tiff stored in fname"
  (let ((captured-image
          (#_CGWindowListCreateImage
           #$CGRectNull
           #$kCGWindowListOptionIncludingWindow
           (#/windowNumber (cocoa-ref (get-front-window)))
           #$kCGWindowImageBoundsIgnoreFraming)))
    (let ((ns-image
            (#/initWithCGImage:size: (#/alloc ns:ns-image)
             captured-image
             #$NSZeroSize)))
      (let ((tiff-rep 
              (#/TIFFRepresentation ns-image)))
        (#/writeToFile:atomically: tiff-rep
         (objc:make-nsstring fname)
         #$NO)))))
