(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cocoa)
  (require :easygui)
  (require :resources)
  (require :mcl-ccl-colors))

; These are shorthand guard macros for usual cases. Only use these if you quickly want to add
; a guard statement with minimal useful error messages. Otherwise, use the guard macro and 
; provide a more meaningful error message

(defmacro guard-!null-ptr (&body body)
  `(guard ((not (equal it1 ccl:+null-ptr+)) "null ptr returned when evaling form ~a" ',body)
     (progn ,@body)))

(defmacro guard-!nil (&body body)
  `(guard (it1 "nil returned when evaling form ~a" ',body)
     (progn ,@body)))

(defmacro guard-nil (&body body)
  `(guard ((null it1) "~a returned when evaling form ~a; expected nil" it1 ',body)
     (progn ,@body)))

(defmacro guard-t-or-nil (&body body)
  `(guard ((or (eq it1 nil) (eq it1 t)) "~a returned when evaling form ~a: expected t or nil" it1 ',body)
     (progn ,@body)))

(defmacro! return-time-ms (&body body)
  `(let ((,g!ctime (get-internal-real-time)))
     ,@body
     (* (/ (- (get-internal-real-time) ,g!ctime) internal-time-units-per-second)
        1000)))

; ----------------------------------------------------------------------
; Building class definitions to match MCL's GUI class hierarchy
;
; Most of the class definitions used by MCL are available in CCL using
; the easygui package. However, a few of the slot initargs in the easygui
; package do not match up with MCL initargs. So for these, use mixin
; classes that override initargs in easygui with initargs that match
; MCL's spec.
; ----------------------------------------------------------------------

(defconstant $tejustleft :left)
(defconstant $tejustcenter :center)
(defconstant $tejustright :right)
(defparameter *screen-width* (easygui::screen-width))
(defparameter *screen-height* (easygui::screen-height))

(defclass view-text-via-title-mixin (easygui::view-text-via-title-mixin)
  ((easygui::text :initarg :window-title)))

(defclass view-text-via-button-title-mixin (view-text-via-title-mixin)
  ())

(defclass view-text-via-stringvalue-mixin (easygui::view-text-via-stringvalue-mixin)
  ((easygui::text :initarg :text)))

(defclass view-text-mixin (easygui::view-text-mixin)
  ((text-justification :accessor text-justification :initarg :text-justification :initform $tejustleft)))

; TODO: Should this default font spec be used for the default initarg to :view-font (as defined in view-mixin)?
(defparameter *fred-default-font-spec* (list "profont" 9 :PLAIN))

(defclass view-mixin (easygui:view)
  ((easygui::size :initarg :view-size)
   (easygui::position :initarg :view-position :initform (make-point 0 0))
   (temp-view-subviews :initarg :view-subviews)
   (easygui::foreground :initform (color-symbol->system-color 'black))
   (easygui::background :initform (make-color 0 0 0 0.0)))
  (:default-initargs :view-font '("Monaco" 9 :SRCOR :PLAIN (:COLOR-INDEX 0))))

; MCL allows for subviews to be passed at object initialization. I tried shadowing the 'easygui::subviews :initargs symbol
; with :view-subviews, so that MCL code cleanly initialized easygui's subviews slot, but it turns out that this slot isn't always 
; where the subviews are supposed to go. If the view is a window, then the subviews go as subviews under the content-view slot.
; easygui handles all of this in their add-subviews method, so the technique here is to use a temp slot on the view-mixin class,
; make that :initarg :view-subviews, and then on object initialization, take any provided subviews and call easygui's add-subviews method
; on them. Then clear the temp slot. It's a hack, but it seems to work, and requires minimal code additions and still uses
; easygui's add-subviews machinery, etc.

(defmethod initialize-instance :after ((view view-mixin) &key) 
  (when (slot-boundp view 'temp-view-subviews)
    (apply #'add-subviews view (slot-value view 'temp-view-subviews))
    (slot-makunbound view 'temp-view-subviews)))

; Try to keep the class hierarchy of the public interface the same as it is for MCL.
; So, simple-view is top; then view (allows subviews); then types that inherit from view,
; like, window, dialog stuff, etc.

(defclass simple-view (easygui::simple-view view-mixin output-stream pen-mixin)
  ((bezier-path :accessor bezier-path :initform nil)
   (direction :initarg :direction :initform :output)
   (wptr :initarg :wptr :initform nil)
   (help-spec :initarg :help-spec :initform nil)
   (view-scroll-position :initarg :view-scroll-position :initform (make-point 0 0))))

(defmethod view-default-size ((view simple-view))
  (make-point 100 100))

(defmethod parse-mcl-initargs (&rest list)
  (loop for (keyword val . rest) in list
        when val append (apply #'parse-mcl-initarg keyword val rest)))

; easygui expects the font slot to be initialized with an ns-font type. However, MCL uses the
; same slot name and expects the font slot to be initialized with a font spec as a list.
; So in order to make it so that the font slot is correct for easygui, shadow the :view-font
; initarg if it is provided by the equivalent ns-font value

(defmethod initialize-instance :around ((view simple-view) &rest args &key back-color view-font view-size view-position view-scroll-position)
  (let ((accum (parse-mcl-initargs
                 (list :back-color back-color)
                 (list :view-font view-font)
                 (list :view-size view-size)
                 (list :view-position view-position)
                 (list :view-scroll-position view-scroll-position))))
    (apply #'call-next-method view (nconc accum args))))

(defmethod initialize-instance :after ((view simple-view) &key)
  (with-slots (direction wptr view-scroll-position) view
    (guard ((eq direction :output) "only :output for direction slot is allowed"))
    (guard ((eq wptr nil) "only nil for wptr slot is allowed"))
    (guard ((points-equal-p view-scroll-position
                            (make-point 0 0)) "non-(0,0) view-scroll-position is not currently implemented"))))

; Parsing MCL initarg lists, and converting to CCL/Easygui equivalents

(defun make-font (name pt)
  (guard ((not (equal it1 ccl:+null-ptr+)) "font not found for font-name ~a" name)
    (#/fontWithName:size: ns:ns-font
     (objc:make-nsstring name)
     pt)))

(defun color-lst->color (lst)
  (destructuring-bind (type val) lst
    (ecase type
      (:color (mcl-color->system-color val))
      (:color-index
        (unless (eq val 0)
          (error "need to support this")
          ; Default, so return nil
          ())))))

(defmethod parse-mcl-initarg ((keyword (eql :view-font)) font-lst &key)
  (let ((name) (pt) (color))
    (dolist (atom font-lst)
      (etypecase atom
        (string (setf name atom))
        (integer (setf pt atom))
        ; FIXME; Parse these style and transfer mode values
        (keyword ())
        (list (setf color (color-lst->color atom)))))
    (nconc
      (list :view-font (make-font name pt))
      (if color
        (list :fore-color color)))))

(defmethod parse-mcl-initarg ((keyword (eql :back-color)) back-color &key)
  (list :back-color (mcl-color->system-color back-color)))

(defmethod parse-mcl-initarg ((keyword (eql :view-size)) view-size &key)
  (list :view-size (mcl-point->system-point view-size)))

(defmethod parse-mcl-initarg ((keyword (eql :view-position)) view-position &key size)
  (list :view-position
        (if size
          (parse-view-position-argument view-position size)
          (mcl-point->system-point view-position))))

(defmethod parse-view-position-argument ((keyword (eql :centered)) size)
  (destructuring-bind (sizex sizey) (as-list (mcl-point->system-point size))
    (make-point (/ (- *screen-width* sizex) 2)
                (/ (- *screen-height* sizey) 2))))

(defmethod parse-view-position-argument ((keyword list) size)
  (guard ((eq t easygui::*screen-flipped*)))
  (destructuring-bind (keyword offset) keyword
    (destructuring-bind (x y) (as-list (parse-view-position-argument :centered size))
      (destructuring-bind (sizex sizey) (as-list (mcl-point->system-point size))
        (ecase keyword
          (:top (setf y offset))
          (:bottom (setf y (- *screen-height*
                              (+ sizey offset))))
          (:left (setf x offset))
          (:right (setf x (- *screen-width*
                             (+ sizex offset)))))
        (make-point x y)))))

(defmethod parse-view-position-argument ((keyword easygui::eg-point) size)
  (declare (ignore size))
  keyword)

(defmethod parse-view-position-argument ((keyword integer) size)
  (declare (ignore size))
  keyword)

(defmethod parse-mcl-initarg ((keyword (eql :view-scroll-position)) view-scroll-position &key)
  (list :view-scroll-position (mcl-point->system-point view-scroll-position)))

(defclass view (simple-view)
  ()
  (:documentation "Top-level class for views"))

(defclass contained-view (easygui::contained-view view)
  ((easygui::background :initform (color-symbol->system-color 'white))))

(defclass static-view-mixin (easygui::static-view-mixin) ())

(defclass window (easygui:window view-text-via-title-mixin view)
  ((grow-icon-p :initform nil :initarg :grow-icon-p :reader grow-icon-p)
   (grow-box-p :initarg :grow-box-p)
   (theme-background :initarg :theme-background)
   (window-show :initarg :window-show)
   (window-type :initarg :window-type)
   (close-box-p :accessor close-box-p :initarg :close-box-p :initform t)
   (maintenance-thread :accessor maintenance-thread)
   (initialized-p :accessor initialized-p :initform nil)
   (easygui::background :initform (color-symbol->system-color 'white))
   (close-requested-p :accessor close-requested-p :initform nil)
   (window-close-fct :reader window-close-fct :initform #'easygui:perform-close)
   (sema-finished-close :accessor sema-finished-close :initform (make-semaphore))
   (sema-request-close :accessor sema-request-close :initform (make-semaphore))
   (window-do-first-click :initarg :window-do-first-click :initform nil)
   (window-other-attributes :initarg :window-other-attributes :initform 0)
   (process :initarg :process :initform nil)
   (auto-position :initarg :auto-position :initform nil))
  (:default-initargs 
    :view-position (make-point 200 200)
    :view-size (make-point 200 200)
    :contained-view-specifically 'contained-view))

(defun process-active-p (p)
  (ccl::process-active-p p))

(defmethod initialize-instance :after ((win window) &key)
  (with-slots (window-do-first-click window-other-attributes process auto-position) win
    (guard ((null window-do-first-click) "non-nil window-do-first-click not currently implemented"))
    (guard ((eq 0 window-other-attributes) "non-zero window-other-attributes not currently implemented"))
    (guard ((null process) "process slot should be nil"))
    (guard ((member auto-position (list nil :noAutoCenter)) "auto-position not currently implemented")))
  (setf (maintenance-thread win)
        (process-run-function 
          (format nil "maintenance thread for win ~a" win)
          (lambda ()
            (setf (initialized-p win) t)
            (while (wptr win)
              (cond ((close-requested-p win)
                     (sv-log "closing ~a on thread ~a~%" win *current-process*)
                     ; easygui's perform-close currently runs on current thread; maintenance thread does 
                     ; not have an autorelease-pool set up; so explicitly create one for the close
                     (easygui::with-autorelease-pool
                       (funcall (window-close-fct win) win))
                     (signal-semaphore (sema-finished-close win)))
                    ((aand (front-window) (eq win it))
                     (window-null-event-handler win)))
              (timed-wait-on-semaphore (sema-request-close win) .1))))))

; Give each window a maintenance thread. In that thread,
; periodically check if the window is the frontmost window.
; If it is, call window-null-event-handler on the window. 

; I took a sample of the refresh rate of MCL's
; calls to window-null-event-handler, and it
; was around 100ms. So using that rate here.
(defmethod initialize-instance :around ((win window) &rest args &key view-position view-size)
  (let ((accum (when (and view-position view-size)
                 (parse-mcl-initarg :view-position view-position :size view-size))))
    (apply #'call-next-method win (nconc accum args))))

(objc:defmethod (#/close :void) ((self easygui::cocoa-window))
  (let ((win (easygui::easygui-window-of self)))
    (slot-makunbound win 'easygui::ref)
    (call-next-method)))

(defparameter *window-null-event-handler-lock* (make-lock "window-null-event-handler-lock")) 

(defmethod window-null-event-handler ((win window))
  ())

(defmethod window-null-event-handler :around ((win window))
  (cond ((try-lock *window-null-event-handler-lock*)
         (unwind-protect (call-next-method)
           (release-lock *window-null-event-handler-lock*)))
        (t
         (sv-log "not calling null-event-handler for win ~a because another null-event-handler is active~%" win))))

(defmethod window-close ((win window))
  (unless (wptr win)
    (sv-log "Attempting to close window ~a which is already closed" win)
    (return-from window-close nil))
  (when (close-requested-p win)
    (sv-log "Already requested for window ~a to be closed" win)
    (return-from window-close nil))
  (setf (close-requested-p win) t)
  (signal-semaphore (sema-request-close win))
  (sv-log "requesting to close win ~a on thread ~a~%" win *current-process*)
  (let ((time
          (return-time-ms
            (timed-wait-on-semaphore (sema-finished-close win) .5))))
    (sv-log "waited for ~,2f ms before win ~a was closed by maintenance thread~%" time win)))

(defclass static-contained-view (static-view-mixin contained-view) ())

(defclass static-window (static-view-mixin window)
  ()
  (:default-initargs :contained-view-specifically 'static-contained-view)) 

(defclass not-closable-window-mixin (window)
  ((window-close-fct :initform (lambda (win) (#/close (cocoa-ref win))))))

(defclass windoid (not-closable-window-mixin window)
  ((easygui::level :initform 1)
   (easygui::resizable-p :initform nil)
   (easygui::minimizable-p :initform nil)
   (easygui::closable-p :initform nil)))

(defclass borderless-window (not-closable-window-mixin window)
  ((easygui::resizable-p :initform nil)
   (easygui::minimizable-p :initform nil)
   (easygui::closable-p :initform nil)
   (easygui::style :initform #$NSBorderlessWindowMask)))

(defmethod windoid-p ((win t))
  nil)

(defmethod windoid-p ((win windoid))
  t)

(defclass simple-overlay-view (easygui::overlay-view view) 
  ()
  (:documentation "Top-level class for views that do not monitor mouse clicks and mouse movement"))

(defclass consuming-view (easygui::consuming-view view)
  ())

(defclass color-dialog (window)
  ()
  (:documentation "Top-level class for windows"))

(defclass liner (view)
  ((liner-type :reader liner-type :initarg :liner-type)
   (easygui::foreground :reader color :initarg :color)))

(defclass dialog (window)
  ()
  (:default-initargs
    :window-title "Untitled Dialog"
    :window-type :document))

(defclass action-view-mixin (easygui::action-view-mixin) ())

(defmethod initialize-instance :around ((view action-view-mixin) &rest args &key action)
  (let ((accum (parse-mcl-initargs (list :action action :view view))))
    (apply #'call-next-method view (nconc accum args))))

(defmethod parse-mcl-initarg ((keyword (eql :action)) action &key view)
  (guard ((not (null view)) "view must associated with an action"))
  (list :action
        (lambda ()
          (sv-log-n 1 "calling action for ~a" view)
          (funcall action)
          (sv-log-n 1 "finished calling action for ~a" view))))

(defclass dialog-item (view view-text-mixin action-view-mixin)
  ((easygui::dialog-item-enabled-p :initarg :enabled-p)
   (part-color-list :reader part-color-list :initarg :part-color-list)
   (dialog-item-handle :accessor dialog-item-handle :initarg :dialog-item-handle :initform nil)
   (dialog-item-action :accessor dialog-item-action :initarg :dialog-item-action :initform nil)
   (compress-text :accessor compress-text :initarg :compress-text :initform nil)
   (text-truncation :initarg :text-truncation :reader text-truncation :initform #$NSLineBreakByTruncatingTail))
  (:default-initargs 
    :view-font '("Lucida Grande" 13 :SRCCOPY :PLAIN (:COLOR-INDEX 0))))

(defmethod parse-mcl-initarg ((keyword (eql :text-truncation)) val &key)
  (list
    :text-truncation
    (etypecase val
      (keyword (ecase val
                 (:end #$NSLineBreakByTruncatingTail)))
      (integer val))))

(defmethod parse-mcl-initarg ((keyword (eql :dialog-item-action)) action &key view)
  (guard ((not (null view)) "dialog item action must be associated with a view"))
  (list :action (lambda () (funcall action view))))

(defmethod initialize-instance :around ((view dialog-item) &rest args &key text-truncation dialog-item-action)
  (let ((accum (parse-mcl-initargs
                 (list :text-truncation text-truncation)
                 (list :dialog-item-action dialog-item-action :view view))))
    (apply #'call-next-method view (nconc accum args))))

(defmethod initialize-instance :after ((view dialog-item) &key)
  (guard ((null (dialog-item-handle view)) "Not utilizing dialog-item-handle"))
  (guard ((null (compress-text view)) "Not utilizing compress-text"))
  (awhen (text-truncation view)
    (#/setLineBreakMode: (#/cell (cocoa-ref view)) it))
  (when (and (slot-boundp view 'easygui::text)
             (not (slot-boundp view 'easygui::size)))
    (#/sizeToFit (cocoa-ref view))
    (easygui::size-to-fit view))
  (when (slot-boundp view 'part-color-list)
    (loop for (part color) in (group (part-color-list view) 2)
          do (set-part-color view part (mcl-color->system-color color)))))

; Note that the :specifically initarg says what cocoa view class to associate with an instance of the object. 
; These really should have been specified in the easygui package, alongside each easygui class definition IMHO, but they weren't.
; Most of the easygui package uses a global easygui::*view-class-to-ns-class-map* variable that contains mappings of lisp
; classes to cocoa view classes, but I found using this flat mapping to be problematic with clos hierarchies. 
; Easygui also provides a :specifically method to overrule the easygui::*view-class-to-ns-class-map* variable, and I like this better, 
; so I'm using it. The benefits of the :specifically method are: 
; [1] cocoa view class mappings are explicitly written, and contained within each clos class definition. 
; [2] As the clos classes are extended, the :specifically values are inherited/over-ridden in the usual way.

(defclass button-dialog-item (easygui:push-button-view view-text-via-button-title-mixin easygui::text-fonting-mixin dialog-item)
  ((easygui::default-button-p :initarg :default-button)
   (cancel-button :initarg :cancel-button))
  (:default-initargs :specifically 'easygui::cocoa-button :text-justification $tejustcenter))

(defmethod set-fore-color :before ((view view-text-via-button-title-mixin) (color ns:ns-color))
  (let* ((color-title
           (#/initWithAttributedString: (#/alloc ns:ns-mutable-attributed-string)
            (#/attributedTitle (cocoa-ref view))))
         (title-range (ns:make-ns-range 0 (#/length color-title))))
    (#/addAttribute:value:range: color-title
     #$NSForegroundColorAttributeName
     color
     title-range)
    (#/setAttributedTitle: (cocoa-ref view) color-title)))

(defmethod initialize-instance :after ((view view-text-via-button-title-mixin) &key)
  (set-fore-color view (slot-value view 'easygui::foreground)))


(defclass default-button-dialog-item (button-dialog-item)
  ()
  (:default-initargs :dialog-item-text "OK" :default-button t :cancel-button nil))

(defclass static-text-dialog-item (easygui:static-text-view view-text-via-stringvalue-mixin dialog-item)
  ((bordered-p :reader bordered-p)
   (easygui::drawsbackground :initform nil))
  (:default-initargs :specifically 'easygui::cocoa-mouseable-text-field))

; Cocoa doesn't automatically determine the value of drawsbackground dependent on the background color.
; If the back color is clear, drawsbackground should be nil, otherwise t. So if a back-color is passed in
; as an initform, and that color is not clear, inform easygui that the background should be drawn by passing
; a t for :draws-background keyword.

(defmethod initialize-instance :around ((view easygui::background-coloring-mixin) &rest args &key back-color)
  (if (and back-color
           (not (equalp (color-opacity (mcl-color->system-color back-color))
                        0)))
    (apply #'call-next-method view :draws-background t args)
    (call-next-method)))

(defmethod (setf bordered-p) (bordered-p (view static-text-dialog-item))
  (unwind-protect (setf (slot-value view 'bordered-p) bordered-p)
    (#/setBordered: (easygui:cocoa-ref view) (if bordered-p #$YES #$NO))))

(defclass editable-text-dialog-item (easygui:text-input-view view-text-via-stringvalue-mixin dialog-item)
  ((allow-returns :initarg :allow-returns)
   (draw-outline :initarg :draw-outline))
  (:default-initargs :specifically 'easygui::cocoa-text-field))

(defclass radio-button-dialog-item (easygui:radio-button-view view-text-via-button-title-mixin dialog-item)
  ((easygui::cluster :initarg :radio-button-cluster)
   (easygui::selected :initarg :radio-button-pushed-p))
  (:default-initargs :specifically 'easygui::cocoa-button))

(defclass check-box-dialog-item (easygui:check-box-view view-text-via-button-title-mixin dialog-item)
  ((easygui::text :initform ""))
  (:default-initargs :specifically 'easygui::cocoa-button))

(defclass table-dialog-item (view-text-via-stringvalue-mixin dialog-item)
  ())

(defclass sequence-dialog-item (table-dialog-item)
  ((table-sequence :reader table-sequence :initarg :table-sequence)
   (columns :reader columns :initform 1)
   (table-print-function :accessor table-print-function :initarg :table-print-function :initform nil)
   (rows :reader rows)
   (cell-size :reader cell-size :initarg :cell-size)
   (table-hscrollp :reader table-hscrollp :initarg :table-hscrollp :initform nil)
   (table-vscrollp :reader table-vscrollp :initarg :table-vscrollp :initform t)
   (selection-type :initarg :selection-type))
  (:default-initargs :specifically 'easygui::cocoa-matrix))

(defmethod (setf rows) (new-rows (view sequence-dialog-item))
  (with-slots (columns rows) view
    (unwind-protect (setf rows new-rows)
      (#/renewRows:columns: (cocoa-ref view) rows columns))))

(defmethod set-cell-size ((view sequence-dialog-item) h &optional v)
  (destructuring-bind (h v) (canonicalize-point h v)
    (with-slots (cell-size) view
      (unwind-protect (setf cell-size (make-point h v))
        (#/setCellSize: (cocoa-ref view)
         (ns:make-ns-size h v)))))) 

(defmethod cell-contents ((view sequence-dialog-item) h &optional v)
  (destructuring-bind (h v) (canonicalize-point h v)
    (guard ((eq h 0)))
    (nth v (table-sequence view))))

(defmethod set-table-sequence ((view sequence-dialog-item) new-sequence)
  (with-slots (table-sequence table-print-function) view
    (with-accessors ((view-font view-font)) view
      (setf (rows view) (length new-sequence))
      (unwind-protect (setf table-sequence new-sequence)
        (loop for item in new-sequence
              for index from 0
              with cell-array = (#/cells (cocoa-ref view))
              for cell = (#/objectAtIndex: cell-array index)
              do (#/setTitle: cell 
                  (objc::make-nsstring (funcall table-print-function item nil)))
              when view-font do (#/setFont: cell view-font))))))

(defmethod selected-cells ((view sequence-dialog-item))
  (let ((selected-cells))
    (do-array (cell (#/selectedCells (cocoa-ref view)) selected-cells)
      (push-to-end (get-location-of-cell view cell)
                   selected-cells))))

(defmethod get-location-of-cell ((view sequence-dialog-item) (cell ns:ns-cell))
  (rlet ((rownum #>NSInteger)
         (colnum #>NSInteger))
    (#/getRow:column:ofCell: (cocoa-ref view) rownum colnum cell)
    (make-point (pref colnum #>NSInteger)
                (pref rownum #>NSInteger))))

(defclass easygui::cocoa-matrix-cell (easygui::cocoa-extension-mixin ns:ns-text-field-cell)
  ((title-width :accessor title-width))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/setTitleWidth: void) ((self easygui::cocoa-matrix-cell) (width :<CGF>LOAT))
  (setf (title-width self) width))

(objc:defmethod (#/titleWidth: :<CGF>LOAT) ((self easygui::cocoa-matrix-cell) (size :<NSS>IZE))
  (title-width self))

(defmethod initialize-instance :after ((view sequence-dialog-item) &key)
  (let ((cocoa-matrix (cocoa-ref view))
        (prototype (make-instance 'easygui::cocoa-matrix-cell)))
    (with-slots (table-hscrollp table-vscrollp columns) view
      (guard (table-vscrollp "Sequence dialog item must allow vertical scrolling"))
      (guard ((not table-hscrollp) "Sequence dialog item must not allow horizontal scrolling"))
      (guard ((eq columns 1) "Only supporting a single column currently")))
    (unless (table-print-function view)
      (setf (table-print-function view)
            (lambda (item strm) (format strm "~a" item))))
    (unless (slot-boundp view 'cell-size)
      (setf (slot-value view 'cell-size)
            (make-point (point-h (view-size view)) 20)))
    (#/setPrototype: cocoa-matrix prototype)
    (#/setMode: cocoa-matrix #$NSListModeMatrix)
    (#/setIntercellSpacing: cocoa-matrix (ns:make-ns-size 0 0))
    (set-cell-size view (cell-size view))
    (set-table-sequence view (table-sequence view))
    ))

(defclass image-view (easygui::image-view view) ())

(defclass clickable-image-view (easygui::clickable-image-view image-view) ())

(defclass back-image-view (image-view) ())

(defclass icon-dialog-item (clickable-image-view dialog-item view)
  ((icon :reader icon :initarg :icon)
   (easygui::view-text :initarg :view-text)))

(defun icon->pict-id (icon)
  (format nil "~a" icon))

(defmethod initialize-instance :after ((view icon-dialog-item) &key)
  (when (slot-boundp view 'icon)
    (#/setImage: (easygui:cocoa-ref view)
     (get-resource-val (icon->pict-id (icon view)) 'image))))

(defclass image-view-mixin ()
  ((pict-id :reader pict-id :initarg :pict-id)
   (image-view :accessor image-view)))

(defmethod (setf pict-id) (pict-id (view image-view-mixin))
  (unwind-protect (setf (slot-value view 'pict-id) pict-id)
    (#/setImage: (easygui:cocoa-ref (image-view view)) (get-resource-val pict-id 'image))))

(defmethod initialize-instance :after ((view image-view-mixin) &key)
  (let ((image-view (make-instance 'back-image-view
                                   :view-size (view-size view)
                                   :view-position (make-point 0 0))))
    (setf (image-view view) image-view)
    (add-subviews view image-view)
    (when (slot-boundp view 'pict-id)
      (#/setImage: (easygui:cocoa-ref image-view) (get-resource-val (pict-id view) 'image)))))

; Place all images in the background (behind all other views). Do this by
; specializing on the add-1-subview method in the easygui package. And call
; cocoa's method for adding a subview that is behind all other views

(defmethod easygui::add-1-subview ((view back-image-view) (super-view view))
  (setf (slot-value view 'easygui::parent) super-view)
  (push view (slot-value super-view 'easygui::subviews))
  (#/addSubview:positioned:relativeTo: 
   (easygui:cocoa-ref super-view) 
   (easygui:cocoa-ref view)
   #$NSWindowBelow
   nil))

(defmethod easygui::add-1-subview :after ((view image-view) (super-view view))
  (unless (slot-boundp view 'easygui::size)
    (let ((ns-size (#/size (#/image (cocoa-ref view)))))
      (destructuring-bind (width height) (list
                                           (ns:ns-size-width ns-size)
                                           (ns:ns-size-height ns-size))
        (setf (slot-value view 'easygui::size) (make-point width height))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (provide :icon-dialog-item))

(defun make-dialog-item (class position size text &optional action &rest attributes)
  ; easygui's action slot takes a lambda with zero arguments; MCL's action slots take a lambda 
  ; with the object/view as an argument. So to enable this feature in easygui, wrap the provided lambda
  ; in a closure that takes zero arguments. 
  ;
  ; To build the closure, allocate storage for a variable first, then set the value of that variable to the created 
  ; instance, but within that instance, use the reference to the value before the value is actually updated. 
  ; This technique is actually wrapped up in a macro called alet in Hoyte's book, but I'm not using the macro here.
  (let ((obj))
    (setf obj (apply #'make-instance class
                     (nconc
                       (list
                         :view-position position
                         :view-size size
                         :text text)
                       (if action (list :dialog-item-action action))
                       attributes)))
    obj))


(defclass menu-view (easygui::menu-view view view-text-via-title-mixin easygui::decline-menu-mixin)
  ((easygui::text :initarg :menu-title)
   (default-item :initarg :default-item :initform 1)
   (auto-update-default :initarg :auto-update-default)
   (item-display :initarg :item-display))
  (:default-initargs :specifically 'easygui::cocoa-pop-up-button))

; FIXME: menu-item-checked isn't being used at all; default-item from menu-view determines which item is checked. Is it worth the time
; to use this slot, and make it so that the char rendered for the checked item can be changed, or also that multiple items can be checked,
; etc.?

(defclass menu-item (easygui::menu-item-view view view-text-via-title-mixin action-view-mixin easygui::decline-menu-mixin)
  ((easygui::text :initarg :menu-item-title)
   (style :initarg :style)
   (menu-item-checked :initarg :menu-item-checked :initform nil))
  (:default-initargs :specifically 'easygui::cocoa-menu-item))

(defmethod initialize-instance :around ((view menu-view) &rest args &key default-item menu-items)
  (if menu-items
    (apply #'call-next-method view :selection (nth (1- default-item) menu-items) args)
    (call-next-method)))

(defclass pop-up-menu (easygui::pop-up-menu menu-view) ())

; ----------------------------------------------------------------------
; Building methods that allow CCL to understand basic MCL drawing commands

; Many of the functions/methods for basic MCL drawing are available in CCL's 
; easygui package. For the functions, import them into the current package.
; For the methods, add a generic method to the current CCL package
; that calls the generic method in the easygui package. Don't import the 
; generic functions from the easygui package, because this will cause 
; symbol collisions for the generic methods in the current package that are
; already defined (might be because they are an act-r interface method, or
; because they are an already-defined CCL method)
; ----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-function 'point-v) #'easygui:point-y)
  (setf (symbol-function 'point-h) #'easygui:point-x)
  (shadowing-import 'easygui:point-x)
  (shadowing-import 'easygui:point-y))

(ccl::register-character-name "UpArrow" #\U+F700)
(ccl::register-character-name "DownArrow" #\U+F701)
(ccl::register-character-name "BackArrow" #\U+F702)
(ccl::register-character-name "ForwardArrow" #\U+F703)
(ccl::register-character-name "CheckMark" #\t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *arrow-cursor* (#/arrowCursor ns:ns-cursor))
  (defparameter *crosshair-cursor* (#/crosshairCursor ns:ns-cursor))
  (defparameter *i-beam-cursor* (#/IBeamCursor ns:ns-cursor))
  (defparameter *black-pattern* 'black-pattern-fixme))

(defun make-point (x y)
  (easygui::point x y :allow-negative-p t))

(defmethod add-points ((p1 easygui::eg-point) (p2 easygui::eg-point))
  (make-point
    (+ (point-x p1) (point-x p2))
    (+ (point-y p1) (point-y p2))))

(defmethod subtract-points ((p1 easygui::eg-point) (p2 easygui::eg-point))
  (make-point
    (- (point-x p1) (point-x p2))
    (- (point-y p1) (point-y p2))))

(defmethod as-list ((p easygui::eg-point))
  (list (point-h p)
        (point-v p)))

(defmethod points-equal-p ((p1 easygui::eg-point) (p2 easygui::eg-point))
  (destructuring-bind (x1 y1) (as-list p1)
    (destructuring-bind (x2 y2) (as-list p2)
      (and (eq x1 x2)
           (eq y1 y2)))))

; This reverse engineering was done entirely by pattern matching.
; I need to learn more about how bit shifting works if I want to 
; be completely confident that this implementation is correct.

(defun mcl-point-h (pt)
  (logand (lsh pt 0) #xffff))

(defun mcl-point-v (pt)
  (logand (lsh pt -16) #xffff))

(defun mcl-point->system-point (mcl-point)
  (etypecase mcl-point
    (integer (make-point
               (mcl-point-h mcl-point)
               (mcl-point-v mcl-point)))
    (easygui::eg-point mcl-point)))

(defmethod point-string ((point easygui::eg-point))
  (format nil "#@(~a ~a)" (point-x point) (point-y point)))

(defmethod add-subviews ((view simple-view) &rest subviews)
  (when subviews
    (apply #'easygui:add-subviews view subviews)))

(defmethod remove-subviews ((view simple-view) &rest subviews)
  (when subviews
    (apply #'easygui:remove-subviews view subviews)))

(defmethod subviews ((view simple-view) &optional subview-type)
  (declare (ignore subview-type))
  (easygui:view-subviews view))

(defmethod view-subviews ((view simple-view))
  (easygui:view-subviews view))

(defmethod view-named (name (view simple-view))
  (acond ((easygui:view-named name view)
          it)
         (t
           (sv-log "no subview with view-nick-name ~a found in ~a" name view)
           nil)))

(defmethod find-named-sibling ((view simple-view) name)
  (let ((container (view-container view)))
    (and container (view-named name container))))

(defmethod view-nick-name ((view simple-view))
  (easygui:view-nick-name view))

(defmethod window-select ((win window))
  (easygui:window-show win))

(defmethod window-show ((win window))
  (easygui:window-show win))

(defmethod window-hide ((win window))
  (easygui::window-hide win))

(defmethod window-shown-p ((window window))
  (not (easygui::window-hidden window)))

(defun find-window (title &optional class)
  (let ((title (format nil "~a" title)))
    (do-array (cocoa-win (#/windows (#/sharedApplication ns:ns-application)))
      (when (easygui::cocoa-win-p cocoa-win)
        (let* ((wintitle (objc:lisp-string-from-nsstring (#/title cocoa-win)))
               (clos-win (easygui::easygui-window-of cocoa-win))
               (winclass (class-name (class-of clos-win)))
               (prefix (subseq wintitle 0 (min (length wintitle) (length title)))))
          (when (string-equal prefix title)
            (when (or (not class) (eq class winclass))
              (return-from find-window clos-win)))))))
  nil)

(defun front-window (&key class include-invisibles include-windoids)
  (guard ((null include-invisibles)))
  (when class 
    (when (symbolp class)
      (setq class (find-class class)))
    (when (class-inherit-from-p class (find-class 'windoid))
      (setq include-windoids t)))
  (objc:with-autorelease-pool
    (let ((wins (gui::windows)))
      (dolist (win wins)
        (when (easygui::cocoa-win-p win)
          (let ((wob (easygui::easygui-window-of win)))
            (when (and wob
                       (initialized-p wob)
                       (or include-windoids
                           (not (windoid-p wob)))
                       (or (null class)
                           (inherit-from-p wob class)))
              (return wob))))))))

(defun inherit-from-p (ob parent)
  (ccl::inherit-from-p ob parent))

(defun class-inherit-from-p (class parent-class)
  (flet ((get-class (value)
           (if (symbolp value) (find-class value nil) value)))
    (let ((pclass (get-class parent-class)))
      (memq pclass
        (ccl::%inited-class-cpl (get-class class))))))

;FIXME: This looks very strange. Prob related to Phaser's floating window
(defun ccl::window-bring-to-front (w &optional (wptr (wptr w)))
  #-:sv-dev (declare (ignore wptr))
  nil)

(defmethod set-window-layer ((window window) new-layer &optional include-invisibles)
  #-:sv-dev (declare (ignore new-layer include-invisibles))
  'fixme)

(defmethod window-title ((view window))
  ;TODO: Maybe use easygui:view-text method here?
  (easygui::window-title view))

(defmethod dialog-item-text ((view view-text-mixin))
  (easygui:view-text view))

(defmethod set-dialog-item-text ((view view-text-mixin) text)
  (setf (easygui:view-text view) text))

(defmethod text-just ((view view-text-mixin))
  (text-justification view))

(defun convert-justification (justification)
  (let ((mapping (list (cons $tejustleft #$NSLeftTextAlignment)
                       (cons nil #$NSLeftTextAlignment)
                       (cons $tejustcenter #$NSCenterTextAlignment)
                       (cons $tejustright #$NSRightTextAlignment))))
    (guard (it1 "No mapping found for justification ~a" justification)
      (cdr (assoc justification mapping)))))

(defmethod set-text-justification ((view view-text-mixin) justification)
  (#/setAlignment: (easygui:cocoa-ref view) (convert-justification justification))
  (setf (text-justification view) justification))

(defmethod initialize-instance :after ((view view-text-mixin) &key)
  (set-text-justification view (text-justification view)))

(defmethod set-selection-range ((view view-text-mixin) &optional position cursorpos)
  (destructuring-bind (position cursorpos) (if position
                                             (list position cursorpos)
                                             (list 0 0))
    ; In order for setSelectedRange: to work, the view must be selected first, so the 
    ; view is currently selected by calling selectText:, which actually highlights all
    ; text in the view. So, a bit of a kludge, but it seems to behave just fine.
    (#/selectText: (cocoa-ref view)
     ccl:+null-ptr+)
    (#/setSelectedRange:
     (#/fieldEditor:forObject: (cocoa-ref (guard-!nil (view-window view)))
      #$YES 
      (cocoa-ref view))
     (ns:make-ns-range position (- cursorpos position)))))

(defmethod dialog-item-enable ((view action-view-mixin))
  (easygui:set-dialog-item-enabled-p view t))

(defmethod dialog-item-disable ((view action-view-mixin))
  (easygui:set-dialog-item-enabled-p view nil))

(defmethod check-box-check ((item check-box-dialog-item))
  (easygui:check-box-check item nil))

(defmethod check-box-uncheck ((item check-box-dialog-item))
  (easygui:check-box-uncheck item nil))

(defmethod check-box-checked-p ((item check-box-dialog-item))
  (easygui:check-box-checked-p item))

(defmethod radio-button-unpush ((item radio-button-dialog-item))
  (easygui:radio-button-deselect item))

(defmethod radio-button-push ((item radio-button-dialog-item))
  (easygui:radio-button-select item))

(defmethod radio-button-pushed-p ((item radio-button-dialog-item))
  (easygui:radio-button-selected-p item))

(defmethod view-position ((view simple-view))
  (easygui:view-position view))

(defmethod view-position :before ((window window))
  (let ((frame (#/frame (cocoa-ref window))))
    (let ((position 
            (make-point
              (ns:ns-rect-x frame)
              (easygui::convert-if-screen-flipped
                (ns:ns-rect-y frame)
                (point-y (view-size window))))))
      (setf (slot-value window 'easygui::position) position))))

; FIXME: This seems to work properly, but I don't currently understand why,
; or what view-origin is supposed to do in MCL
(defmethod view-origin ((view simple-view))
  (let ((bounds (#/bounds (cocoa-ref view))))
    (make-point (ns:ns-rect-x bounds)
                (ns:ns-rect-y bounds))))

(defmethod origin ((view simple-view))
  (view-origin view))

(defmethod set-origin ((view simple-view) h &optional v)
  (destructuring-bind (h v) (canonicalize-point h v)
    (#/setBoundsOrigin: (cocoa-ref view) (ns:make-ns-point h v))))

; Note that this is MCL's arglist spec. The erase-p isn't needed for CCL,
; but it should be kept here so that MCL code calling invalidate-view still works.
(defmethod invalidate-view ((view simple-view) &optional erase-p)
  (declare (ignore erase-p))
  (easygui:invalidate-view view))

; FIXME: What is validate-view supposed to do differently than invalidate-view?
; And is that difference already handled within cocoa?
(defmethod validate-view ((view simple-view))
  (invalidate-view view))

(defun canonicalize-point (x y)
  (cond (y (list x y))
        (t (list (point-h x) (point-v x)))))

(defmethod set-view-position ((view simple-view) x &optional (y nil))
  (destructuring-bind (x y) (canonicalize-point x y)
    (let ((pos (make-point x y)))
      (setf (easygui:view-position view) pos))))

(defmethod set-view-size ((view simple-view) x &optional (y nil))
  (destructuring-bind (x y) (canonicalize-point x y)
    (let ((size (make-point x y)))
      (setf (easygui:view-size view) size))))

(defmethod view-size ((view simple-view))
  (easygui:view-size view))

(defmethod width ((view simple-view))
  (point-h (view-size view)))

(defmethod height ((view simple-view))
  (point-v (view-size view)))

(defmethod view-container ((view simple-view))
  (easygui:view-container view))

(defmethod view-window ((view simple-view))
  (easygui::easygui-window-of view))

(defmethod content-view ((view window))
  (easygui:content-view view))

(defmethod content-view ((view simple-view))
  view)

; Other MCL drawing methods are not available in the easygui package.
; For these, move down a layer below easygui, and implement the functionality
; using CCL's Objective C bridge. Most bridge calls will have #/ or #_ reader
; macros in the expression

; A few with- macros to handle setup/teardown, and make programming a bit easier

; This one uses Doug Hoyte's "defmacro!" and ",g!" syntax to easily handle unwanted variable capture. 
(defmacro! with-graphics-context (&body body)
  "Any changes to the graphics environment by body, will be valid only in body"
  `(let ((,g!context (#/currentContext ns:ns-graphics-context)))
     (unwind-protect (progn 
                       (#/saveGraphicsState ,g!context)
                       ,@body)
       (#/restoreGraphicsState ,g!context))))

; ----------------------------------------------------------------------
; Section to handle current focused view and font focused view.
; 
; The dynamic variables are used to keep track of any views that are focused in the dynamic environment.
; Code could call with-focused-view explicitly, or a view might become focused because code called paint-rect
; and passed a view to that function. The goal is to have the code do the right thing and try to
; figure out which view has focus. If it can't figure this out, then an exception will be thrown.
; These can be seen where the guard macros are used.
; ----------------------------------------------------------------------

(defparameter *current-graphics-context-stroke-color* nil)

(defmacro! with-fore-color (o!color &body body)
  `(progn
     (guard ((eq (type-of ,g!color) 'ns:ns-color) "color ~a is not a system color" ,g!color) ())
     (let ((*current-graphics-context-stroke-color* ,g!color))
       (with-graphics-context
         (#/set ,g!color)
         ,@body))))

(defmacro with-fallback-fore-color (color &body body)
  `(if (null *current-graphics-context-stroke-color*)
     (with-fore-color ,color
       ,@body)
     (progn ,@body)))

(defmacro with-window-fallback-fore-color (view &body body)
  `(with-fallback-fore-color (get-fore-color (view-window ,view))
     ,@body))

(defparameter *current-focused-view* nil)
(defparameter *current-font-view* nil)
(defparameter *current-graphics-context-font* nil)

(defmacro! with-focused-view (o!view &body body)
  "Any changes to the graphics environment by body will be directed to the view object"
  `(let ((*current-focused-view* (content-view ,g!view)))
     (easygui:with-focused-view (easygui:cocoa-ref (content-view ,g!view))
       ,@body)))

(defmacro! with-font-view (o!view &body body)
  `(let ((*current-font-view* ,g!view))
     ,@body))

(defmacro! with-font-focused-view (o!view &body body)
  `(with-font-view ,g!view
     (with-focused-view ,g!view
       ,@body)))

(defmacro with-fallback-focused-view (view &body body)
  `(if (null *current-focused-view*)
     (with-focused-view ,view
       ,@body)
     (progn ,@body)))

(defmacro with-fallback-font-view (view &body body)
  `(if (null *current-font-view*)
     (with-font-view ,view
       ,@body)
     (progn ,@body)))

(defmacro! with-fallback-font-focused-view (o!view &body body)
  `(with-fallback-font-view ,g!view
     (with-fallback-focused-view ,g!view
       ,@body)))

(defmacro with-window-of-focused-view-fallback-fore-color (&body body)
  `(with-window-fallback-fore-color (guard-!nil *current-focused-view*)
     ,@body))

(defmacro with-font-view-fallback-font (&body body)
  `(let ((*current-graphics-context-font* (view-font (guard-!nil *current-font-view*))))
     ,@body))

(defmethod wptr ((view window))
  (if (slot-boundp view 'easygui::ref)
    (#/isVisible
     (guard-!null-ptr
       (easygui::cocoa-ref view)))))

(defmethod local-to-global ((view simple-view) local-pos)
  (add-points (view-position view) local-pos))

(defmethod part-color ((view view-text-mixin) (part (eql :text)))
  (declare (ignore part))
  (get-fore-color view))

(defmethod set-part-color ((view dialog-item) (part (eql :body)) new-color)
  (set-back-color view new-color))

(defmethod set-part-color ((view dialog-item) (part (eql :text)) new-color)
  (set-fore-color view new-color))

; FIXME: Keep this as a compiler warning until you figure out how to color a border with Cocoa
(defmethod set-part-color ((view dialog-item) (part (eql :frame)) new-color)
  #-:sv-dev (declare (ignore new-color))
  (setf (bordered-p view) t))

(defmethod get-fore-color ((view simple-view))
  (easygui:get-fore-color view))

(defmethod get-back-color ((view view))
  (easygui:get-back-color view))

(defmethod set-fore-color ((view simple-view) new-color)
  (easygui:set-fore-color view new-color))

(defmethod set-back-color ((view simple-view) new-color)
  (easygui:set-back-color view new-color))

; FIXME: What does this do? Keep as compiler warning until you figure it out
(defmethod window-update-cursor ((window window) point)
  #-:sv-dev (declare (ignore point))
  nil)

; Handling mouse movement/interaction

(defmethod easygui::mouse-down :after ((view simple-view) &key location &allow-other-keys)
  (let ((win (guard-!nil
               (guard-!null-ptr
                 (view-window view)))))
    (view-click-event-handler view location)
    (view-click-event-handler win location)
    (post-view-click-event-handler win location)))

(defmethod post-view-click-event-handler ((view window) position)
  (declare (ignore position))
  (values))

(defmethod view-click-event-handler :around ((device simple-view) position)
  (declare (ignore position))
  (sv-log-n 1 "starting view-click-event-handler for ~a" device)
  (unwind-protect (call-next-method)
    (sv-log-n 1 "ending view-click-event-handler for ~a" device)))

(defmethod view-click-event-handler ((device simple-view) position)
  (declare (ignore position))
  ; Default primary method is to do nothing
  (values))

(defmethod view-mouse-position ((view simple-view))
  (easygui:view-mouse-position view :allow-negative-position-p t))

(defmacro with-psn (&body body)
  `(rlet ((psn #>ProcessSerialNumber))
     (#_GetFrontProcess psn)
     ,@body))

(defun create-mouse-event (event pos)
  (#_CGEventCreateMouseEvent
   ccl:+null-ptr+
   event
   pos
   0))

(defun left-mouse-up (pos)
  (let ((event
          (create-mouse-event #$NSLeftMouseUp pos)))
    (sv-log-n 1 "posting mouse-up event ~a" event)
    (#_CGEventPost 0 event) 
    (sv-log-n 1 "releasing mouse-up event ~a" event)
    (#_CFRelease event)))

(defun left-mouse-down (pos)
  (let ((event
          (create-mouse-event #$NSLeftMouseDown pos)))
    (sv-log-n 1 "posting mouse-down event ~a" event)
    (#_CGEventPost 0 event) 
    (sv-log-n 1 "releasing mouse-down event ~a" event)
    (#_CFRelease event)))

; It takes roughly 1 ms for an event to hit the application's run loop, so sleep for 50x 
; longer than this, to make extra extra sure that the event has hit the run loop before returning.

(defun left-mouse-click (pos &optional (delay t))
  (sv-log-n 1 "starting left mouse click")
  (easygui::running-on-main-thread ()
    (let ((pos (easygui::ns-point-from-point pos)))
      (left-mouse-down pos)
      (left-mouse-up pos)))
  (sv-log-n 1 "sleeping so that mouse click enters nsrun loop")
  (when delay (spin-for-fct 50))
  (sv-log-n 1 "ending left mouse click"))

; Handling keyboard interaction

(defun create-keyboard-event (event key)
  (let ((key (format nil "~a" key)))
    (guard ((eq (length key) 1) "key: ~a is not a single character; not supporting command/control key events" key) ())
    (let ((ret (#_CGEventCreateKeyboardEvent
                ccl:+null-ptr+
                0
                event)))
      (#_CGEventKeyboardSetUnicodeString
       ret
       (length key)
       (#/cStringUsingEncoding: (objc:make-nsstring key) #$NSUTF8StringEncoding))
      ret)))

(defun keypress-down (key)
  (let ((event 
          (create-keyboard-event #$YES key)))
    (sv-log-n 1 "posting keypress-down event ~a" event)
    (#_CGEventPost 0 event) 
    (sv-log-n 1 "releasing keypress-down event ~a" event)
    (#_CFRelease event)))

(defun keypress-up (key)
  (let ((event 
          (create-keyboard-event #$NO key)))
    (sv-log-n 1 "posting keypress-up event ~a" event)
    (#_CGEventPost 0 event) 
    (sv-log-n 1 "releasing keypress-up event ~a" event)
    (#_CFRelease event)))

; Same sleep time here.

(defun keypress (key &optional (delay t))
  (sv-log-n 1 "starting keypress")
  (easygui::running-on-main-thread ()
    (keypress-down key)
    (keypress-up key))
  (sv-log-n 1 "sleeping so that keypress enters nsrun loop")
  (when delay (spin-for-fct 50))
  (sv-log-n 1 "ending keypress"))

(defmethod easygui::view-key-event-handler :after ((device window) key)
  (view-key-event-handler device key)
  (post-view-key-event-handler device key))

(defmethod post-view-key-event-handler ((device window) key)
  (declare (ignore key))
  (values))

(defmethod view-key-event-handler :around ((device window) key)
  (declare (ignore key))
  (sv-log-n 1 "starting view-key-event-handler")
  (unwind-protect (call-next-method)
    (sv-log-n 1 "ending view-key-event-handler")))

(defmethod view-key-event-handler ((device window) key)
  (declare (ignore key))
  ; Default primary method on the window is to do nothing
  (values))

; MCL's Pen

(defclass pen-mixin ()
  ((pen-mode :accessor pen-mode)
   (pen-size :accessor pen-size)
   (pen-position :accessor pen-position :initform (make-point 0 0))
   (pen-pattern :accessor pen-pattern)))

(defmethod initialize-instance :after ((view pen-mixin) &key)
  (pen-normal view))

(defmethod set-pen-mode ((view simple-view) newmode)
  (setf (pen-mode view) newmode))

(defmethod set-pen-pattern ((view simple-view) newpattern)
  (setf (pen-pattern view) newpattern))

(defmethod set-pen-size ((view simple-view) h &optional v)
  (destructuring-bind (h v) (canonicalize-point h v)
    (setf (pen-size view) (make-point h v))))

(defmethod pen-normal ((view simple-view))
  (setf (pen-mode view) :patCopy)
  (setf (pen-size view) (make-point 1 1))
  (setf (pen-pattern view) *black-pattern*))

; ----------------------------------------------------------------------
; Triggering MCL's view-draw-contents method on a Cocoa redraw of views.
;
; CCL's Objective C bridge provides an interface to define objective c 
; methods. Use this to define a method that will be called any time
; an object on the screen needs to be redrawn. This will in turn call
; view-draw-contents in CCL, which means that the way to describe how an 
; object is drawn in CCL is the same way that it is in MCL: Add a view-draw-contents
; method that dispatches on the object type, and code to draw that type of object
; to the display
; ----------------------------------------------------------------------


; Note that Cocoa focuses the view before calling #/drawRect, so there's no reason to have a
; with-focused-view inside of the lisp code. But in order for the with-fallback-focused-view stuff to 
; work, it needs to know that a view is already focused. So just set the global var to the view
; in order to do this.

(objc:defmethod (#/drawRect: :void) ((self easygui::cocoa-drawing-view) (rect :<NSR>ect))
  (let* ((view (easygui::easygui-view-of self))
         (*current-focused-view* view))
    (easygui::dcc
      (view-draw-contents view))))

; Drawing methods

(defmacro with-rectangle-arg ((var left &optional top right bottom) &body body)
  `(let ((,var (make-rect :from-mcl-spec ,left ,top ,right ,bottom)))
     ,@body))

(defmethod make-rect ((mode (eql :from-mcl-spec)) &rest args)
  (destructuring-bind (left top right bottom) args
    (destructuring-bind (left top right bottom) (canonicalize-rect left top right bottom)
      (destructuring-bind (startx starty width height) (list left top (- right left) (- bottom top))
        (ns:make-ns-rect startx starty width height)))))

(defun canonicalize-rect (left top right bottom)
  (cond (bottom (list left top right bottom))
        (top (list (point-h left)
                   (point-v left)
                   (point-h top)
                   (point-v top)))
        (t (list (ns:ns-rect-x left)
                 (ns:ns-rect-y left)
                 (+ (ns:ns-rect-x left) (ns:ns-rect-width left))
                 (+ (ns:ns-rect-y left) (ns:ns-rect-height left))))))

(defmethod view-draw-contents ((view simple-view))
  ())

(defmethod get-start ((view liner))
  (get-start-using-liner-type view (liner-type view)))

(defmethod get-end ((view liner))
  (get-end-using-liner-type view (liner-type view)))

(defmethod get-start-using-liner-type ((view liner) (liner-type (eql 'bu)))
  (make-point 0 (point-y (view-size view))))

(defmethod get-start-using-liner-type ((view liner) (liner-type (eql 'td)))
  (make-point 0 0))

(defmethod get-end-using-liner-type ((view liner) (liner-type (eql 'bu)))
  (make-point (point-x (view-size view)) 0))

(defmethod get-end-using-liner-type ((view liner) (liner-type (eql 'td)))
  (view-size view))

(defmethod view-draw-contents ((view liner))
  (move-to view (get-start view))
  (with-fore-color (get-fore-color view) 
    (line-to view (get-end view))))

; Drawing commands on windows are directed to the window's content view.
; This is achieved by having with-focused-view focus on the window's content
; view (if it's a window), and also by specializing on the accessors that are 
; used when drawing. The window's pen-position and bezier-path are never used;
; instead, those are directed to the content view of the window.
;
; Another approach would have been to write a specialized method for the window
; for each public drawing method, and have that method call the method with the
; same name on the window's content view, but this would require adding a 
; window-specialized method for each public drawing method. So instead I looked
; at what accessors the public methods are using, and specialized on those, so that
; the necessary code changes for drawing to window's content view could be isolated
; in the few methods below. Adding/removing this functionality can be archived by
; adding/deleting the few methods here.

(defmethod pen-position ((view window))
  (pen-position (content-view view)))

(defmethod (setf pen-position) (new (view window))
  (setf (pen-position (content-view view)) new))

(defmethod bezier-path ((view window))
  (bezier-path (content-view view)))

(defmethod (setf bezier-path) (new (view window))
  (setf (bezier-path (content-view view)) new))

; Actual drawing methods

(defmethod move-to ((view simple-view) x &optional (y nil))
  (with-fallback-focused-view view
    (destructuring-bind (x y) (canonicalize-point x y)
      (qd-move-to x y))))

(defmethod qd-move-to ((val1 (eql :long)) (val2 easygui::eg-point))
  (destructuring-bind (endx endy) (as-list val2)
    (qd-move-to endx endy)))

(defmethod qd-move-to ((x number) (y number))
  (let ((view *current-focused-view*))
    (unless (points-equal-p (pen-position view) (make-point x y))
      (let ((position (make-point x y)))
        (when (bezier-path view)
          (#/moveToPoint: (bezier-path view) (ns:make-ns-point x y)))
        (setf (pen-position view) position)))))

(defmethod line ((view simple-view) x &optional (y nil))
  (with-fallback-focused-view view
    (destructuring-bind (x y) (canonicalize-point x y)
      (line-to view (add-points
                      (pen-position view)
                      (make-point x y))))))

(defmethod line-to ((view simple-view) x &optional (y nil))
  (with-fallback-focused-view view
    (destructuring-bind (endx endy) (canonicalize-point x y)
      (qd-line-to endx endy))))

(defmethod qd-line-to ((val1 (eql :long)) (val2 easygui::eg-point))
  (destructuring-bind (endx endy) (as-list val2)
    (qd-line-to endx endy)))

(defmethod qd-line-to ((endx number) (endy number))
  (let ((view *current-focused-view*))
    (unless (points-equal-p (pen-position view) (make-point endx endy))
      (destructuring-bind (startx starty) (list (point-x (pen-position view))
                                                (point-y (pen-position view)))
        (when (bezier-path view)
          (#/lineToPoint: (bezier-path view) (ns:make-ns-point endx endy)))
        (setf (pen-position view) (make-point endx endy))
        (with-window-of-focused-view-fallback-fore-color
          (#/strokeLineFromPoint:toPoint:
           ns:ns-bezier-path
           (ns:make-ns-point startx starty) 
           (ns:make-ns-point endx endy)))))))

(defmethod frame-oval ((view simple-view) left &optional top right bottom)
  (let* ((rect (make-rect :from-mcl-spec left top right bottom))
         (path (#/bezierPathWithOvalInRect: ns:ns-bezier-path rect)))
    (with-fallback-focused-view view
      (with-window-of-focused-view-fallback-fore-color
        (#/stroke path)))))

(defmethod paint-oval ((view simple-view) left &optional top right bottom)
  (with-fallback-focused-view view
    (fill-oval view (pen-pattern view) left top right bottom)))

(defmethod fill-oval ((view simple-view) pattern left &optional top right bottom)
  #-:sv-dev (declare (ignore pattern))
  (let* ((rect (make-rect :from-mcl-spec left top right bottom)))
    (with-fallback-focused-view view
      (qd-paint-oval rect))))

(defmethod qd-paint-oval ((rect ns:ns-rect))
  (let ((path (#/bezierPathWithOvalInRect: ns:ns-bezier-path rect)))
    (with-window-of-focused-view-fallback-fore-color
      (#/fill path))))

(defmethod frame-rect ((view simple-view) left &optional top right bottom)
  (let* ((rect (make-rect :from-mcl-spec left top right bottom)))
    (with-fallback-focused-view view
      (qd-frame-rect rect))))

(defmethod qd-frame-rect ((rect ns:ns-rect))
  (with-window-of-focused-view-fallback-fore-color
    (#/strokeRect: ns:ns-bezier-path rect)))

(defmethod paint-rect ((view simple-view) left &optional top right bottom)
  (with-fallback-focused-view view
    (fill-rect view (pen-pattern view) left top right bottom)))

(defmethod fill-rect ((view simple-view) pattern left &optional top right bottom)
  (with-fallback-focused-view view
    (let* ((rect (make-rect :from-mcl-spec left top right bottom)))
      (qd-paint-rect rect pattern))))

(defmethod qd-paint-rect ((rect ns:ns-rect) &optional pattern)
  #-:sv-dev (declare (ignore pattern))
  (with-window-of-focused-view-fallback-fore-color
    (#/fillRect: ns:ns-bezier-path rect)))

(defmethod erase-rect ((view simple-view) left &optional top right bottom)
  (let* ((rect (make-rect :from-mcl-spec left top right bottom)))
    (with-fallback-focused-view view
      (with-fore-color (get-back-color (content-view view))
        (qd-paint-rect rect)))))

(defmethod start-polygon ((view simple-view))
  (setf (bezier-path view) (#/bezierPath ns:ns-bezier-path))
  (#/retain (bezier-path view))
  (#/moveToPoint: (bezier-path view)
   (easygui::ns-point-from-point (pen-position view))))

(defun pattern->system-color (pattern)
  (color-symbol->system-color
    (guard-!nil
      (cond ((eq pattern *black-pattern*) 'black)))))

(defmethod paint-polygon ((view simple-view) polygon)
  (with-fallback-focused-view view
    (fill-polygon view (pen-pattern view) polygon)))

(defmethod erase-polygon ((view simple-view) polygon)
  (with-fallback-focused-view view 
    (with-fore-color (get-back-color (content-view view))
      (fill-polygon view (pen-pattern view) polygon))))

(defmethod fill-polygon ((view simple-view) pattern polygon)
  #-:sv-dev (declare (ignore pattern))
  (with-fallback-focused-view view
    (with-window-of-focused-view-fallback-fore-color
      (#/fill polygon))))

(defmethod frame-polygon ((view simple-view) polygon)
  (with-fallback-focused-view view
    (with-window-of-focused-view-fallback-fore-color
      (#/stroke polygon))))

(defmethod kill-polygon ((polygon ns:ns-bezier-path))
  (#/release polygon)
  (setf polygon nil))

(defmethod get-polygon ((view simple-view))
  (prog1 (bezier-path view)
    (start-polygon view)))

; FIXME: Currently it's expected that a format call to a view is done only once per view-draw-contents. So write
; a single string to the view, etc. But CCL calls write-char when the string has a negative sign at the beginning.
; So the current workaround is to keep a dynamic variable around that keeps track of all of this, and throw in a few
; guard statements to make sure that things are being called in a way that won't break the formatting.

(defparameter *stream-prefix-char* nil)

(defmethod stream-write-char ((v simple-view) char)
  (guard ((null *stream-prefix-char*) "expecting only a single prefix char before the string; prefix was ~a; new char is ~a" *stream-prefix-char* char) ())
  (setf *stream-prefix-char* char))

(defun draw-string (string)
  (with-window-of-focused-view-fallback-fore-color
    (with-font-view-fallback-font
      (let ((dict (#/dictionaryWithObjectsAndKeys: ns:ns-mutable-dictionary
                   *current-graphics-context-font* #$NSFontAttributeName
                   *current-graphics-context-stroke-color* #$NSForegroundColorAttributeName
                   ccl:+null-ptr+))
            (pt (pen-position *current-focused-view*)))
        (unwind-protect (#/drawAtPoint:withAttributes: string
                         (ns:make-ns-point
                           (point-h pt)
                           ; To mimic MCL positioning, I had to subtract of the ascend pixels from the y position of the pen
                           (- (point-v pt)
                              (first (multiple-value-list (font-info *current-graphics-context-font*)))))
                         dict)
          (setf *stream-prefix-char* nil))))))

(defmethod stream-write-string ((v simple-view) string &optional start end)
  (with-fallback-font-focused-view v
    (let* ((string
             (objc:make-nsstring
               (format nil "~a~a" (aif *stream-prefix-char* it "")
                       (if start
                         (subseq string start end)
                         string)))))
      (draw-string string))))

; Handling fonts and string width/height in pixels

(defmethod view-font ((view simple-view))
  (guard-!null-ptr
    (guard-!nil
      (easygui:view-font view))))

(defun font-info (font-spec)
  (values (guard-!null-ptr (#/ascender font-spec))
          (abs (guard-!null-ptr (#/descender font-spec)))))

(defun get-dict-for-font (font)
  (#/dictionaryWithObjectsAndKeys: ns:ns-mutable-dictionary
   font #$NSFontAttributeName 
   ccl:+null-ptr+))

(defun string-width (str font)
  (let* ((dict (get-dict-for-font font))
         (attr (#/initWithString:attributes: (#/alloc ns:ns-attributed-string)
                (objc:make-nsstring str)
                dict))
         (size (#/size attr)))
    (ns:ns-size-width size)))

(defun font-point (font)
  (#/pointSize font))

(defun font-name (font)
  (objc:lisp-string-from-nsstring (#/fontName font)))

; Miscellaneous wrappers

; Provide the :quickdraw package on *modules*. Keeps from having to comment out the (require :quickdraw) lines in the MCL code.
; Also since the file here implements the quickdraw interface, it makes since to announce that the quickdraw library is available to use.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (provide :quickdraw))

; To implement event-dispatch for Clozure, send a dummy function over to
; the main Cocoa thread to be evaluated, and block until that function is 
; processed. This guarantees that all current event code in the Cocoa run loop
; has been processed before event-dispatch returns.

(defun event-dispatch ()
  (sv-log-n 1 "starting event dispatch")
  (let ((time
          (return-time-ms
            (unless (eq ccl::*current-process* ccl::*initial-process*)
              (dotimes (i 2)
                (let ((sema (make-semaphore)))
                  (gui::queue-for-gui 
                    (lambda ()
                      (signal-semaphore sema))
                    :at-start nil)
                  (wait-on-semaphore sema nil "semaphore event-dispatch wait")))))))
    (sv-log-n 1 "ending event dispatch after ~,2f ms" time)))

(defparameter *current-dialog-directory* nil)

(defun get-directory-with-fallback (directory)
  (setf *current-dialog-directory*
        (acond (directory it)
               (*current-dialog-directory* it)
               (*load-truename* (directory-namestring it))
               (t nil))))

; It turns out that objc functions are defined in the symbol table. So in order to set the title of
; the panel that is opened in the dialog, dynamically shadow the #/openPanel objc function. 
; And in that shadowed function, call the original, and then set the title of the resulting panel to prompt

(defun make-panel-and-set-prompt (fun-orig prompt)
  (lambda (&rest args)
    (let ((panel (apply fun-orig args)))
      (when prompt
        (#/setTitle: panel (objc:make-nsstring prompt)))
      panel)))

(defun choose-file-dialog (&key directory mac-file-type button-string prompt file)
  (with-shadow (#/openPanel (make-panel-and-set-prompt fun-orig prompt))
    (gui::cocoa-choose-file-dialog :directory (get-directory-with-fallback directory)
                                   :file-types (aif mac-file-type (os-type->extensions it))
                                   :file file
                                   :button-string button-string)))

; FIXME: Write this
(defun os-type->extensions (os-type)
  #-:sv-dev (declare (ignore os-type))
  ())

; And use the shadowing technique here.

(defun choose-new-file-dialog (&key directory mac-file-type button-string prompt file)
  #-:sv-dev (declare (ignore button-string))
  (with-shadow (#/savePanel (make-panel-and-set-prompt fun-orig prompt))
    (gui::cocoa-choose-new-file-dialog :directory (get-directory-with-fallback directory)
                                       :file-types (aif mac-file-type (os-type->extensions it))
                                       :file file)))

; And here as well. Except in this case latch into the #/setTitle: method, since that is being used in the
; cocoa-choose-directory-dialog function.

(defun set-title-and-use-prompt (fun-orig prompt)
  (lambda (panel string)
    (funcall fun-orig 
             panel 
             (aif prompt
               (objc:make-nsstring it)
               string))))

(defun choose-directory-dialog (&key directory prompt)
  (with-shadow (#/setTitle: (set-title-and-use-prompt fun-orig prompt))
    (gui::cocoa-choose-directory-dialog :directory (get-directory-with-fallback directory))))

(labels ((gen-dict-for-immutable-attr (bool)
           (#/dictionaryWithObject:forKey: ns:ns-dictionary
            (#/numberWithBool: ns:ns-number bool)
            #$NSFileImmutable))
         (set-immutable-attr (path bool)
           (#/setAttributes:ofItemAtPath:error: (#/defaultManager ns:ns-file-manager)
            (gen-dict-for-immutable-attr bool)
            (objc:make-nsstring path)
            ccl:+null-ptr+)))
  (defun file-locked-p (path)
    (let ((dict (guard-!null-ptr 
                  (#/attributesOfItemAtPath:error: (#/defaultManager ns:ns-file-manager)
                   (objc:make-nsstring path)
                   ccl:+null-ptr+))))
      (guard-t-or-nil
        (#/boolValue
         (#/objectForKey: dict (objc:make-nsstring "NSFileImmutable"))))))
  (defun lock-file (path)
    (let ((path (namestring path)))
      (unless (file-locked-p path)
        (guard-!nil
          (set-immutable-attr path #$YES)))))
  (defun unlock-file (path)
    (let ((path (namestring path)))
      (when (file-locked-p path)
        (guard-!nil
          (set-immutable-attr path #$NO))))))

; FIXME: Write this
(defun set-mac-file-creator (path mac-file-creator)
  (declare (ignore path mac-file-creator))
  t)

; FIXME: And maybe write this
(defun set-mac-file-type (path mac-file-type)
  (declare (ignore path mac-file-type))
  t)

(defparameter *current-cursor* *arrow-cursor*)

(defmethod set-cursor ((cursor ns:ns-cursor))
  (unwind-protect (setf *current-cursor* cursor)
    (awhen (front-window)
      (sv-log "setting cursor for window ~a to ~a" it cursor)
      (#/invalidateCursorRectsForView: (cocoa-ref it)
       (cocoa-ref (content-view it))))))

(objc:defmethod (#/resetCursorRects :void) ((self easygui::cocoa-contained-view))
  (call-next-method)
  (#/addCursorRect:cursor: self
   (#/bounds self)
   *current-cursor*))

; Another option here is to call #/currentCursor on ns-cursor class, but since 
; *current-cursor* is (currently) the current cursor for all windows of the application,
; just use this.

(defmethod window-cursor ((window window))
  *current-cursor*)

(defmethod color ((cursor ns:ns-cursor))
  (guard-!nil
    (cond ((eq cursor *i-beam-cursor*) *black-color*)
          ((eq cursor *arrow-cursor*) *black-color*)
          ((eq cursor *crosshair-cursor*) *black-color*))))

(defmethod create-resource ((type (eql 'cursor)) id)
  (make-instance
    'resource
    :alloc-fn
    (lambda ()
      (#/initWithImage:hotSpot: (#/alloc ns:ns-cursor)
       (get-resource-val id 'image)
       (#/hotSpot *arrow-cursor*)))))

(defmethod get-cursor :before (id)
  (unless (resource-present-p id 'cursor)
    (add-resource
      (create-resource 'cursor id)
      id)))

(defmethod get-cursor (id)
  (get-resource-val id 'cursor))

(defun hide-cursor ()
  (#_CGDisplayHideCursor
   (#_CGMainDisplayID)))

(defun show-cursor ()
  (#_CGDisplayShowCursor
   (#_CGMainDisplayID)))

; Running on main GUI thread is required for the menubar functions. Otherwise Cocoa crashes fairly often when these are called.

(defun hide-menubar ()
  (easygui::running-on-main-thread ()
    (#/setPresentationOptions: (#/sharedApplication ns:ns-application)
     (logior
       #$NSApplicationPresentationHideDock
       #$NSApplicationPresentationHideMenuBar))))

(defun show-menubar ()
  (easygui::running-on-main-thread ()
    (#/setPresentationOptions: (#/sharedApplication ns:ns-application)
     #$NSApplicationPresentationDefault)))

(defun beep ()
  (#_NSBeep))

; ----------------------------------------------------------------------
; Manipulate the read table so that MCL's #@(a b) make-point shorthand works. 
;
; CCL does not support this by default, and the objective-c bridge uses #@ to make an NSString.
; So the #@ read macro defined below performs both tasks: If a list is passed, it 
; will convert the points in the list to a point representation. If a string is
; passed, it will call the CCL default read function for #@
; 
; Examples:
; #@"a string" -> #<NS-CONSTANT-STRING "hello"
; #@(5 4) -> #<EG-POINT (5.00/4.00)>)
; ----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *nonhacked-readtable* (copy-readtable))
  (set-dispatch-macro-character 
    #\# #\@
    (defun |#@-reader| (stream char arg)
      (let ((first-char (peek-char nil stream)))
        (ecase first-char
          (#\( (unless *read-suppress*
                 `(make-point ,@(read stream))))
          (#\" (funcall (get-dispatch-macro-character #\# #\@ *nonhacked-readtable*)
                        stream char arg)))))))

; ----------------------------------------------------------------------
; Manipulate reader functionality so that references to foreign functions that no longer exist can
; be defined as native functions, while keeping the same access syntax
;
; I did not want to have to modify the source code in the Phaser task where all of these carbon foreign 
; functions were used. CCL does not support the carbon framework, as far as I can tell. So in order to 
; trick CCL into thinking that these foreign functions are defined, add a bit of a 'before' section of 
; code to the load-external-function call. If the symbol name of the external function being loaded is
; in the list of function names that are being defined natively, then just return the symbol that maps
; to that function in the function symbol table. Otherwise, call the usual load-external-function function,
; and have CCL do the standard thing to try to find the foreign function
; ----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *load-external-function-orig* #'ccl::load-external-function)
  (with-continue 
    (defun ccl::load-external-function (sym query)
      (let* ((fun-names (list "showmenubar" "hidemenubar" "getcursor" "showcursor" "ShowCursor" "hidecursor" "HideCursor"
                              "paintrect" "PaintRect" "framerect" "drawstring" "moveto" "MoveTo" "lineto" "PaintOval"))
             (the-package (find-package :X86-Darwin64))
             (fun-syms (mapcar (lambda (name)
                                 (intern name the-package))
                               fun-names)))
        (if (member sym fun-syms)
          sym
          (funcall *load-external-function-orig* sym query))))))

; Use the same approach to define foreign constants that MCL uses that no longer exist for CCL

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *load-os-constant-orig* #'ccl::load-os-constant)
  (with-continue
    (defun ccl::load-os-constant (sym &optional query)
      (let* ((con-names (list "tejustleft" "tejustcenter" "tejustright" "crossCursor"))
             (the-package (find-package :X86-Darwin64))
             (con-syms (mapcar (lambda (name)
                                 (intern name the-package))
                               con-names)))
        (if (member sym con-syms)
          sym
          (funcall *load-os-constant-orig* sym query))))))

; All of the functions being natively defined are here

(defun X86-Darwin64::|showmenubar| ()
  (show-menubar))

(defun X86-Darwin64::|hidemenubar| ()
  (hide-menubar))

(defun X86-Darwin64::|getcursor| (id)
  (get-cursor id))

(defun X86-Darwin64::|showcursor| ()
  (show-cursor))

(defun X86-Darwin64::|ShowCursor| ()
  (show-cursor))

(defun X86-Darwin64::|hidecursor| ()
  (hide-cursor))

(defun X86-Darwin64::|HideCursor| ()
  (hide-cursor))

(defun X86-Darwin64::|paintrect| (rect)
  (qd-paint-rect rect))

(defun X86-Darwin64::|PaintRect| (rect)
  (qd-paint-rect rect))

(defun X86-Darwin64::|framerect| (rect)
  (qd-frame-rect rect))

(defun X86-Darwin64::|drawstring| (str)
  (draw-string str))

(defun X86-Darwin64::|lineto| (x y)
  (qd-line-to x y))

(defun X86-Darwin64::|moveto| (x y)
  (qd-move-to x y))

(defun X86-Darwin64::|MoveTo| (x y)
  (qd-move-to x y))

(defun X86-DARWIN64::|PaintOval| (rect)
  (qd-paint-oval rect))

; And the constants are here
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant X86-Darwin64::|tejustleft| $tejustleft)
  (defconstant X86-Darwin64::|tejustcenter| $tejustcenter)
  (defconstant X86-Darwin64::|tejustright| $tejustright)
  (defconstant X86-Darwin64::|crossCursor| *crosshair-cursor*))
