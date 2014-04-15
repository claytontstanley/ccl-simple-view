(defvar *response* nil)
(defvar *model* nil)

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  ;(break)
  (setf *response* (string key))
  (clear-exp-window)
  (format t "Pressed key: ~s~%" key)
  (when *model* 
    (proc-display)))

(defmethod rpm-window-click-event-handler ((win rpm-window) pos)
  (format t "position: ~s~%" pos)
  ;(break)
  (setf *pos* pos))

(defun do-demo2 (&optional who)
  
  (reset)

   (if (eq who 'human)
      (setf *model* nil)
    (setf *model* t))
  
  (setf s1 *standard-output*)
  (setf s2 (make-string-output-stream))

  (let* ((lis (permute-list '("B" "C" "D" "F" "G" "H" 
                              "J" "K" "L" "M" "N" "P" 
                              "Q" "R" "S" "T" "V" "W" 
                              "X" "Y" "Z")))
         (text1 (first lis))
         (window (open-exp-window "Letter recognition")))
    
    (add-text-to-exp-window :text text1 :x 125 :y 150)
    
    (setf *response* nil) 
         
    (if *model*
        (progn
          (install-device window)
          (proc-display)
          (run 10 :real-time t))
      
      (while (null *response*)
        (allow-event-manager window)))
    
    (get-output-stream-string s2)))



(clear-all)

(define-model demo2

(sgp :seed (123456 0))
(sgp :v t :needs-mouse t :show-focus t :trace-detail high)
  
(chunk-type read-letters state)
(chunk-type array letter loc)

(add-dm 
 (start isa chunk) (attend isa chunk)
 (respond isa chunk) (done isa chunk)
 (goal isa read-letters state start))

(P find-unattended-letter
   =goal>
      ISA         read-letters
      state       start
 ==>
   +visual-location>
      ISA         visual-location
      :attended    nil
   =goal>
      state       find-location
)

(P attend-letter
   =goal>
      ISA         read-letters
      state       find-location
   =visual-location>
      ISA         visual-location
   
   ?visual>
      state       free
   
==>
   +visual>
      ISA         move-attention
      screen-pos  =visual-location
   =goal>
      state       attend
)

(P encode-letter
   =goal>
      ISA         read-letters
      state       attend
   =visual>
      ISA         text
      value       =letter
   ?imaginal>
      state       free
==>
   =goal>
      state       respond
   +imaginal>
      isa         array
      letter      =letter
      loc         =visual
)


(P respond
   =goal>
      ISA         read-letters
      state       respond
   =imaginal>
      isa         array
      letter      =letter
   ?manual>   
      state       free
==>
   =imaginal>
   =goal>
      state       done
   +manual>
      ISA         press-key
      key         =letter
)

(p to-mouse
  =goal>
   isa read-letters
   state done
  ?manual>
   state free
==>
  +manual>
   isa hand-to-mouse
  =goal>
   state move)

(p move
  =goal>
   isa read-letters
   state move
  =imaginal>
   isa array
   loc =loc
 ?manual>
   state free
==>
  +manual>
   isa move-cursor
   object =loc
  =goal>
   state click)
(p click
  =goal>
   isa read-letters
   state click
  
 ?manual>
   state free
==>
  +manual>
   isa click-mouse
  -goal>)
(goal-focus goal)

)

#|
(do-demo2)
(clear-all)
(running-on-main-thread () (make-instance 'rpm-real-window))
*listener-output*
|#
