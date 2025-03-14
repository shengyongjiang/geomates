;;;
;;; example dummy agent
;;;  

(clear-all)

;;; in the agent, arbitrary helper functions may be defined
;;; using Common Lisp, but also all add-ons of the SBCL lisp
;;; system, in particular loading shared libraries and calling
;;; functions in those libraries.
;;; For details, see SBCL manual regarding its alien function interace
;;; or have a look into geomates.lisp which connects to a C library
;;;
;;; Additionally, you can use run-program to call any external software.
;;; Note that the process will be run in a null environment by default, so
;;; all pathnames must be explicit. To handle different locations, a simple
;;; "or" may be all it takes:

(defparameter *my-ls* (or (probe-file "/bin/ls")
			  (probe-file "/usr/bin/ls")
			  (probe-file "some/path"))
  "binds to the first file that exists")

(defun count-entries ()
  "counts the number of files/directories in the root directory"
  (count #\Newline ; just count linebreaks since after printing a name, ls prints a newline
	 (with-output-to-string (result) ; temporary string output stream
	   (run-program (probe-file "/bin/ls") (list "/") :output result))))

;;; In case you need to differentiate different environments/OS/compilers:
;;; have a look at Common-Lisps reader macros #+/#- (like #ifdef in C),
;;; which refer to the global variable *features*
;;; examples:
;;; #+SBCL (print "I'm running the SBCL compiler")
;;; (defparameter *magic-code* #+LITTLE-ENDIAN #x0f12 #-LITTLE-ENDIAN 0x120f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions section

;; comment it because it load from sbcl already
;; (load "navigation-functions.lisp") 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; Now comes the core Act-R agent
;;;

(define-model lost-agent

  ;; [find explanation in actr7.x/examples/vision-module]
  (chunk-type (polygon-feature (:include visual-location)) regular)
  (chunk-type (polygon (:include visual-object)) sides)
  
  ;; [see definition in vision module]
  ;;(chunk-type (oval (:include visual-object)) (oval t))
  
  ;; [might be obsolete] Do this to avoid the warnings when the chunks are created
  (define-chunks true false polygon)
  
  ;; [might be obsolete] stuff the leftmost item
  (set-visloc-default screen-x lowest)

  ;; Set global parameters
    (sgp 
        :step t
        :trace-detail high
        :v t
        :real-time 300
    )        

  (chunk-type goal state intention)
  (chunk-type control intention button)

    ;; who-i-am: the name of the agent, could be "yellow-disc" or "red-rect"
    ;; phase 0: initial status
    ;; phase 10: found yellow disc
    ;; phase 20: found red block
    ;; phase 30: moving left
    ;; phase 40: found yellow disc after moving
    ;; phase 50: retrieved previous position record and compared with current position

    ;; disc speed: 0 not moving, 1 moving right once 2 move right twice etc, and - is moving left
    ;; block expand: 0 not expand, 1 expand  once 2 expand twice etc

    ;;; todo : clean up  who-i-am phase slot
    (chunk-type position-record 
                                is-disc speed  disc-x disc-y 
                                is-rect expand rect-x rect-y
                                diamond-x diamond-y
                                phase
                                )

  (add-dm
   (move-left) (move-right)
   (move-up)  (move-down)
   (w) (a) (s) (d)
   (i-dont-know-where-to-go)
   (something-should-change)
   (i-want-to-do-something)
   (i-dont-know-who-i-am)
   (yellow-disc)
   (red-rect)
   (up-control isa control intention move-up button w)
   (down-control isa control intention move-down button s)
   (left-control isa control intention move-left button a)
   (right-control isa control intention move-right button d)

   (position-record-chunk isa position-record 
                            is-disc 0 speed 0 disc-x nil disc-y nil 
                            is-rect 0 expand 0 rect-x nil rect-y nil 
                            diamond-x nil diamond-y nil 
                            phase 0)

   (first-goal isa goal state i-dont-know-who-i-am)
   (second-goal isa goal state searching-for-diamond)
   (third-goal isa goal state dummy-moving-right-up intention test-initialize)
   ;; (first-goal isa goal state i-dont-know-where-to-go disc-x nil disc-y nil rect-x nil rect-y )
   )

   (goal-focus first-goal)
;;   (goal-focus second-goal)
  ;;(goal-focus third-goal)
  
;; Step 1: Record the initial position of the yellow disc
(p find-yellow-disc
    =goal>
        state i-dont-know-who-i-am
    ?visual>
        state free
    ?imaginal>
        state free
==>
    +visual-location>
        value       "disc"
        :attended   nil
    +imaginal>
        isa position-record
        phase 0
    =goal>
        intention searching-for-yellow-disc
    !output! ("---- 1.1.1 Searching for yellow disc with specific criteria")
)

;; Only proceed to attend when we have a visual location
(p attend-to-yellow-disc
    =goal>
        state       i-dont-know-who-i-am
        intention   searching-for-yellow-disc
    =visual-location>
        screen-x =x
        screen-y =y
    ?visual>
        state free
    =imaginal>
==>
    +visual>
        cmd move-attention
        screen-pos =visual-location
    =imaginal>
        speed 0
        disc-x =x
        disc-y =y
        phase 10
    =goal>
        intention   ready-to-find-red-rect
    !output! ("---- 1.1.2 Moving attention to object at x: ~S y: ~S" =x =y)
)
  
;; Step 1: Record the initial position of the yellow disc
(p find-red-rect
    =goal>
        state       i-dont-know-who-i-am
        intention   ready-to-find-red-rect
    ?visual>
        state free
    ?imaginal>
        state free
==>
    +visual-location>
        value       "rect"
        :attended   nil
    =goal>
        intention   searching-for-red-rect
    !output! ("---- 1.1.3 Searching for red block with specific criteria")
)

;; Only proceed to attend when we have a visual location
(p attend-to-red-rect
    =goal>
        state       i-dont-know-who-i-am
        intention   searching-for-red-rect
    =visual-location>
        screen-x =x
        screen-y =y
    ?visual>
        state free
    =imaginal>
==>
    +visual>
        cmd move-attention
        screen-pos =visual-location
    =imaginal>
        expand 0
        rect-x =x
        rect-y =y
        phase 30
    =goal>
        intention   ready-to-move-right
    !output! ("---- 1.1.4 Moving attention to object at x: ~S y: ~S" =x =y)
)

(p move-right-to-location
    =goal>
        state       i-dont-know-who-i-am
        intention   ready-to-move-right
    =imaginal>
    ?manual>
        state free
==>
    =goal>
        intention   ready-re-find-yellow-disc
    +manual>
        cmd press-key
        key d
        duration 0.5
    =imaginal>
    !output! ("---- 1.1.5 Moving right")
)

(p ready-re-find-yellow-disc
    =goal>
        state       i-dont-know-who-i-am
        intention   ready-re-find-yellow-disc
    ?visual>
        state free
==>
    +visual-location>
        value "disc"
        ;; :attended nil         ;; Look for unattended disc
    =goal>
        intention   searching-for-yellow-disc-after-move
    !output! ("---- 1.1.6 Re-Find yellow disc after moving")
)

(p re-attend-to-yellow-disc-no-change
    =goal>
        state       i-dont-know-who-i-am
        intention   searching-for-yellow-disc-after-move
    =visual-location>
        screen-x =new-disc-x
    =imaginal>
        isa position-record
        disc-x =old-disc-x
        disc-x =new-disc-x
    ?visual>
        state free
==>
    +visual>
        cmd move-attention  
        screen-pos =visual-location
    =goal>
        state       i-dont-know-who-i-am
        intention   ready-re-find-red-rect
    =imaginal>
        disc-x =new-disc-x
    !output! ("---- 1.1.7 Found disc at position: x=~S (disc position unchanged from ~S)" =new-disc-x =old-disc-x)
)

(p re-attend-to-yellow-disc-changed
    =goal>
        state       i-dont-know-who-i-am
        intention   searching-for-yellow-disc-after-move
    =visual-location>
        screen-x =new-disc-x
    =imaginal>
        isa position-record
        disc-x =old-disc-x
        - disc-x =new-disc-x
    ?visual>
        state free
==>
    +visual>
        cmd move-attention  
        screen-pos =visual-location
    =goal>
        state       collecting-diamond
        intention   searching-diamond
    =imaginal>
        disc-x =new-disc-x
        is-disc 1
        is-rect 0
    !output! ("---- 1.1.7 Found disc at new position: x=~S (disc position changed from ~S)" =new-disc-x =old-disc-x)
)

(p ready-re-find-red-rect
    =goal>
        state       i-dont-know-who-i-am
        intention   ready-re-find-red-rect
    ?visual>
        state free
==>
    +visual-location>
        value "rect"
    =goal>
        intention   searching-for-red-rect-after-move
    !output! ("---- 1.1.8 Re-Find red block after moving")
)

(p re-attend-to-red-rect-no-change
    =goal>
        state       i-dont-know-who-i-am
        intention   searching-for-red-rect-after-move
    =visual-location>
        screen-x =new-rect-x
    =imaginal>
        isa position-record
        rect-x =old-rect-x
        rect-x =new-rect-x
    ?visual>
        state free
==>
    +visual>
        cmd move-attention  
        screen-pos =visual-location
    =goal>
        intention   ready-re-find-yellow-disc
    =imaginal>
        rect-x =new-rect-x
    !output! ("---- 1.1.9 Found red block at position: x=~S (block position unchanged from ~S)" =new-rect-x =old-rect-x)
)

(p re-attend-to-red-rect-changed
    =goal>
        state       i-dont-know-who-i-am
        intention   searching-for-red-rect-after-move
    =visual-location>
        screen-x =new-rect-x
    =imaginal>
        isa position-record
        rect-x =old-rect-x
        - rect-x =new-rect-x
    ?visual>
        state free
==>
    +visual>
        cmd move-attention  
        screen-pos =visual-location
    =goal>
        state       collecting-diamond
        intention   searching-diamond
    =imaginal>
        rect-x =new-rect-x
        is-rect 1
        is-disc 0
    !output! ("---- 1.1.9 Found red block at new position: x=~S (block position changed from ~S)" =new-rect-x =old-rect-x)
)

;; todo: clean up maybe this is not necessary
(p find-next-action-if-neither
    =goal>
        state       i-dont-know-who-i-am
        intention   searching-for-yellow-disc-after-move
    =imaginal>
        is-disc =0
        is-rect =0
==>
    =imaginal>
    =goal>
        state ready-to-move-right
    !output! ("---- 1.1.10 Don't know I am yellow disc or red block, move right again to find")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find nearest diamond

;; adapter for stepper
;; (p find-next-action-shall-search-for-diamond
;;     =goal>
;;         state       collecting-diamond
;;         intention   searching-diamond
;; ==>
;;     =goal>
;;         state       collecting-diamond
;;         intention   searching-diamond
;;     !output! ("---- 2.0.0 Searching for diamond")
;; )

(p find-diamond-locations
    =goal>
        state       collecting-diamond
        intention   searching-diamond
    ?visual>
        state free
==>
    +visual-location>
        value "diamond"
        :attended   nil
    =goal>
        intention evaluating-diamond-locations
    !output! ("---- 2.1.1 Requesting locations of any diamonds")
)

(p attend-closest-diamond
    =goal>
        state       collecting-diamond
        intention   evaluating-diamond-locations
    =visual-location>
        screen-x =any-diamond-x
        screen-y =any-diamond-y
    ?imaginal> 
        state free
    =imaginal>
        isa position-record
    ?visual>
        state free
    ;; - visual-location>  ;; Ensure this is the first match (simplistic closest check)
    ;; todo : maybe we could start any diamond location, search for the nearest one anyway
==>
    +visual>
        cmd move-attention
        screen-pos =visual-location
    =goal>
        state       deciding-next-action
        ;; intention   ready-to-move-right
        intention   test-intention
    =imaginal>
        isa position-record
        diamond-x =any-diamond-x
        diamond-y =any-diamond-y
    !output! ("---- 2.1.2 Attending to diamond at x=~S, y=~S" =any-diamond-x =any-diamond-y)
)

;; break point production
(p decide-next-action-disc
    =goal>
        state       deciding-next-action
        intention   test-intention
    ?imaginal>
        state free
    =imaginal>
        isa position-record
        is-disc =1
        disc-x =dx
        disc-y =dy
        diamond-x =diamx
        diamond-y =diamy
==>
    !bind! =next-move-state (find-next-action-disc =dx =dy =diamx =diamy)
    =imaginal>
    =goal>  
        state       =next-move-state
        intention   nil
    !output! ("---- 2.1.3 next move state is ~S" =next-move-state)
)

(p decide-next-action-rect
    =goal>
        state       deciding-next-action
        intention   test-intention
    ?imaginal>
        state free
    =imaginal>
        isa position-record
        is-rect =1
        rect-x =rx
        rect-y =ry
        diamond-x =diamx
        diamond-y =diamy
==>
    ;; !bind! =next-move-state (find-next-action-rect =rx =ry =diamx =diamy)
    !bind! =next-move-state (find-next-action-rect =rx =ry 60 26)
    =imaginal>
    =goal>  
        state       =next-move-state
        intention   nil
    !output! ("---- 2.1.3 next move state is ~S" =next-move-state)
)

;; (p find-next-action-if-disc
;;     =goal>
;;         state find-next-action
;;     =imaginal>
;;         is-disc =1
;; ==>
;;     =imaginal>
;;     =goal>
;;         state       dummy-moving-right-up
;;         intention   move-right
;;     !output! ("---- 1.2.1 Ready for random moving right jump")
;; )

;; (p find-next-action-if-block
;;     =goal>
;;         state find-next-action
;;     =imaginal>
;;         is-rect =1
;; ==>
;;     =imaginal>
;;     =goal>
;;         state       dummy-moving-right-up
;;         intention   move-right
;;     !output! ("---- 1.2.2 Ready for implment rect movement")
;; )


;; Step 3 random moving right jump
;; todo  only for testing
;; (p initialize-imaginal
;;     =goal>
;;         state dummy-moving-right-up
;;         intention test-initialize
;;     ?imaginal>
;;         state free
;;         buffer empty
;; ==>
;;     =goal>
;;         state dummy-moving-right-up
;;         intention move-right
;;     +imaginal>
;;         isa position-record
;;         speed 0
;;     !output! ("---- 3.0.1 Initializing imaginal buffer with starting values")
;; )

(p perform-move-right
    =goal>
        state       dummy-moving-right-up
        intention   nil
    ?imaginal>
        state free
    =imaginal>
        isa position-record
        speed   =current-speed
        ;; is-disc =1
        ;; is-rect =1
    ?manual>
        state free
==>
    !bind! =new-speed (+ =current-speed 1)
    =imaginal>
        speed =new-speed
    +manual>
        cmd press-key
        key d
        duration 0.2
    =goal>
        state       dummy-moving-right-up
        ;; intention   move-up
        intention   update-position-record
    !output! ("---- 3.1.1 Moving right with 'd' (move ~S speed)" =new-speed)
)

(p update-position-record-rect
    =goal>
        ;; state       dummy-moving-right-up
        intention   update-position-record
    ?imaginal>
        state free
    =imaginal>
        isa position-record
        is-rect =1
==>
    +visual-location>
        value       "rect"
    =imaginal>
    =goal>
        intention   searching-for-red-rect
)

(p attend-to-red-rect
    =goal>
        state       i-dont-know-who-i-am
        intention   searching-for-red-rect
    =visual-location>
        screen-x =x
        screen-y =y
    ?visual>
        state free
    =imaginal>
==>
    +visual>
        cmd move-attention
        screen-pos =visual-location
    =imaginal>
        expand 0
        rect-x =x
        rect-y =y
        phase 30
    =goal>
        intention   ready-to-move-right
    !output! ("---- 1.1.4 Moving attention to object at x: ~S y: ~S" =x =y)
)

(p perform-move-up
    =goal>
        state dummy-moving-right-up
        intention move-up
    ?imaginal>
        state free
    =imaginal>
    ?manual>
        state free
==>
    =imaginal>
    +manual>
        cmd press-key
        key w
        duration 2
    =goal>
        state       dummy-moving-right-up
        intention   nil
    !output! ("---- 3.1.2 Moving up, next intention is right")
)

;; (p ready-for-jump-after-move-right
;;     =goal>
;;         state dummy-moving-right-up
;;         intention move-right
;;     =imaginal>
;;         isa position-record
;;         ;; is-disc =1
;;         ;; is-rect =1
;;         speed   =current-speed
;;         > current-speed 3
;;     ?manual>
;;         state free
;; ==>
;;     =goal>
;;         state dummy-moving-right-up
;;         intention jump
;;     =imaginal>
;;     !output! ("---- 3.1.2 Ready for jump after moving right")
;; )

;; (p perform-jump
;;     =goal>
;;         state dummy-moving-right-up
;;         intention jump
;;     =imaginal>
;;         isa position-record
;;         ;; is-disc =1
;;         ;; is-rect =1
;;     ?manual>
;;         state free
;; ==>
;;     =goal>
;;         state dummy-moving-right-up
;;         intention jump-key-pressed
;;     =imaginal>
;;     +manual>
;;         cmd press-key
;;         key w
;;         duration 0.2
;;     !output! ("---- 3.1.3 Performing jump with 'w' after moving right")
;; )

;; (p complete-action
;;     =goal>
;;         state dummy-moving-right-up
;;         intention jump-key-pressed
;;     =imaginal>
;;         ;; isa position-record
;;         ;; is-disc =1
;;         ;; is-rect =1
;;     ?manual>
;;         state free
;; ==>
;;     =goal>
;;         state dummy-moving-right-up
;;         intention move-right
;;     =imaginal>
;;         speed 0
;;     !output! ("---- 3.1.4 Action completed, ready to jump or move right")
;; )

;; Retrieve the previous position record from declarative memory
(p development-mode
    =goal>
        state debug-production
==>
    =goal>
        state debug-production
    !output! ("---- debug-production ----")
    ;; !eval! (dm)  ;; Print all chunks in declarative memory to help debug
)


  (p want-to-move
     =goal>
     state i-want-to-do-something
     intention =intention
     ?retrieval>
     state free
==>
     =goal>
     state something-should-change
     +retrieval>
        intention =intention
     )
  
  (p move
     =goal>
     state something-should-change
     =retrieval>
     button =button
    ?manual>
     state free
 ==>
     =goal>
     state i-dont-know-where-to-go
     +manual>
     cmd press-key
     key =button
     )

  (p retrieval-failure
     =goal>
     state something-should-change
     ?retrieval>
     buffer failure
==>
     =goal>
        state i-dont-know-where-to-go
     )
  
(p maybe-left
    =goal>
        state i-dont-know-where-to-go
    ?manual>
        state free
==>
    =goal>
        state i-want-to-do-something
        intention move-left
)
  
;;   (p maybe-right
;;     =goal>
;;      state i-dont-know-where-to-go
;;      ?manual>
;;      state free
;; ==>
;;      =goal>
;;      state i-want-to-do-something
;;      intention move-right
;; )
  
;;   (p maybe-down
;;      =goal>
;;      state i-dont-know-where-to-go
;;      ?manual>
;;      state free
;; ==>
;;      =goal>
;;         state i-want-to-do-something
;;      intention move-down
;;      )
  
;;   (p maybe-up
;;      =goal>
;;      state i-dont-know-where-to-go
;;      ?manual>
;;      state free
;; ==>
;;     =goal>
;;      state i-want-to-do-something
;;      intention move-up
;;      )
  
  )
