(print 5)

(setf *win* 
      (MAKE-INSTANCE
        'COLOR-DIALOG))

(sleep 2)

(window-close *win*)

