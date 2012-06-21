(make-dialog-item 'icon-dialog-item
                  #@(170 10)
                  #@(64 64)
                  "Untitled"
                  #'(lambda (item)
                      item
                      (break)
                      (format t "Scaling icons doesn't always look great."))
                  :icon 1919905652)
(#/sharedWorkspace ns:ns-workspace)
(#/iconForFileType: (#/sharedWorkspace ns:ns-workspace)
 (#_NSFileTypeForHFSTypeCode #$kComputerIcon))
(#/iconForFileType: (#/sharedWorkspace ns:ns-workspace)
 (#_NSFileTypeForHFSTypeCode 501))
#$kComputerIcon
(setf *image* *)
(setf *view* *)
(inspect *win*)
(#/image (easygui:cocoa-ref *view*))
(#/setImage: (easygui:cocoa-ref *view*) *image*)
(add-subviews *win* *view*)
(remove-all-items-from-rpm-window *win*)
