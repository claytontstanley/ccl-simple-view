
(defclass dummy (image-view-mixin window) ())
(inspect 'initialize-instance)
(find-method #'initialize-instance '(:after) '(image-view))
(remove-method #'initialize-instance *)
(make-instance 'dummy
               :view-position #@(100 100)
               :view-size #@(400 400))
(class-default-initargs (find-class 'dummy))
