
(require :misc-lib)

(defclass blinker ()
  ((container :accessor container :initarg :container :initform nil)
   (views :accessor views :initarg :views :initform nil)
   (delay :accessor delay :initarg :delay :initform 200)
   (last-change :accessor last-change :initarg :last-change
                :initform (get-internal-real-time))
   (blink-p :accessor blink-p :initarg :blink-p :initform nil)
   (post-blink-fct :accessor post-blink-fct :initarg :post-blink-fct
                   :initform nil)
   (name :accessor name :initarg :name :initform nil)
   ))


(defmethod update-blinker ((blnkr blinker))
  (when (and (blink-p blnkr) (views blnkr) (container blnkr)
             (< (delay blnkr) (- (get-internal-real-time) (last-change blnkr))))
    (if (not (view-container (first (views blnkr))))
      (apply #'add-subviews (container blnkr) (views blnkr))
      (progn
        (apply #'remove-subviews (container blnkr) (views blnkr))
        (when (functionp (post-blink-fct blnkr))
          (mapc (post-blink-fct blnkr) (views blnkr)))))
    (setf (last-change blnkr) (get-internal-real-time))))


(defclass blinker-window (window)
  ((blinker-lst :accessor blinker-lst :initarg :blinker-lst
                :initform nil)))

(defmethod window-null-event-handler :after ((wind blinker-window))
  (awhen (blinker-lst wind)
    (mapc #'update-blinker it)))

(defmethod blinker-named ((wind blinker-window) name)
  (first (member name (blinker-lst wind) :key #'name)))

(provide :blinker)


#|
(defmethod toggle-name ((item button-dialog-item))
  (if (string= (dialog-item-text item) "off")
    (set-dialog-item-text item "on")
    (set-dialog-item-text item "off")))

(defmethod toggle-name ((item static-text-dialog-item))
  nil)


(setf wind (make-instance 'blinker-window :view-size #@(400 400)))
(setf tvs (list
            (make-instance 'static-text-dialog-item :view-position #@(20 20)
                           :dialog-item-text "Test")
            (make-instance 'button-dialog-item :view-position #@(100 100)
                           :dialog-item-text "off")))
(setf (blinker-lst wind)
      (list (make-instance 'blinker :views tvs :container wind :blink-p t
                           :post-blink-fct #'toggle-name :name :BOB)))
|#
