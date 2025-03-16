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
                                is-rect expand rect-x rect-y rect-width rect-height
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




    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; ui-procedurals
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 1 nil
    ;; 2 searching-for-yellow-disc 
    ;; 3 ready-to-find-red-rect
    ;; 4 searching-for-rect 
    ;; 5 ready-to-get-rect-size
    ;; 6 update-ui-end
    (p find-yellow-disc
        =goal>
            intention       update-ui
            sub-intention   nil
        ?visual>
            state free
        ?imaginal>
            state free
        =imaginal>
            isa position-record
    ==>
        +visual-location>
            value       "disc"
        =imaginal>
            isa position-record
            phase 0
        =goal>
            sub-intention searching-for-yellow-disc
        !output! ("---- x.0.0 Searching for yellow disc with specific criteria")
    )

    (p attend-to-yellow-disc
        =goal>
            intention       update-ui
            sub-intention   searching-for-yellow-disc
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
            ;; speed 0
            disc-x =x
            disc-y =y
            phase 10
        =goal>
            sub-intention   ready-to-find-red-rect
        !output! ("---- x.1.0 Moving attention to object at x: ~S y: ~S" =x =y)
    )
        
    ;; Step 1: Record the initial position of the yellow disc
    (p find-red-rect
        =goal>
            intention       update-ui
            sub-intention   ready-to-find-red-rect
        ?visual>
            state free
    ==>
        +visual-location>
            value       "rect"
            ;; :attended   nil
        =goal>
            sub-intention   searching-for-rect
            phase 20
        !output! ("---- x.2.0 Searching for red block with specific criteria")
    )

    (p attend-to-red-rect
        =goal>
            intention       update-ui
            sub-intention   searching-for-rect
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
            ;; expand 0
            rect-x =x
            rect-y =y
            phase 30
        =goal>
            sub-intention   ready-to-get-rect-size
        !output! ("---- x.3.0 Moving attention to object at x: ~S y: ~S" =x =y)
    )

    (p get-rect-size
        =goal>
            intention       update-ui
            sub-intention   ready-to-get-rect-size
        =visual>
            width =w
            height =h
        =imaginal>
        ?imaginal>
            state free
    ==>
        =imaginal>
            rect-width =w
            rect-height =h
            phase 40
        =goal>
            sub-intention   update-ui-end
        !output! ("---- x.4.0 Got rectangle size: width=~S height=~S" =w =h)
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    (p i-dont-know-who-i-am
        =goal>
            state i-dont-know-who-i-am
            intention nil
        ?imaginal>
            state free
    ==>
        +imaginal>
            isa position-record
            phase 0
        =goal>
            state           i-dont-know-who-i-am
            intention       update-ui
            sub-intention   nil
        !output! ("---- 1.1.0 Update UI start")    
    )

    (p update-ui
        =goal>
            state           i-dont-know-who-i-am
            intention       update-ui
            sub-intention   update-ui-end
    ==>
        =goal>
            state       i-dont-know-who-i-am
            intention   ready-to-move-right
            sub-intention nil
        !output! ("---- 1.1.1 Update UI end")
    )

    (p move-right-to-location
        =goal>
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
            intention   searching-for-rect-after-move
        !output! ("---- 1.1.8 Re-Find red block after moving")
    )

    (p re-attend-to-rect-no-change
        =goal>
            state       i-dont-know-who-i-am
            intention   searching-for-rect-after-move
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

    (p re-attend-to-rect-changed
        =goal>
            state       i-dont-know-who-i-am
            intention   searching-for-rect-after-move
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

    (p test-code-collecting-diamond
        =goal>
            state       collecting-diamond
            intention   searching-diamond
    ==>
        =goal>
            state           move-right
            intention       update-ui
            sub-intention   nil
        !output! ("---- x.0.0 test code loop right and update UI")
    )

    ;; maybe we should refresh UI
        ;; =goal>
        ;;     state       collecting-diamond
        ;;     intention   searching-diamond

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; find nearest diamond

    
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
            is-disc 1
            disc-x =dx
            disc-y =dy
            diamond-x =diamx
            diamond-y =diamy
    ==>
        !bind! =query-move-state (find-next-action-disc =dx =dy =diamx =diamy)
        =imaginal>
        =goal>  
            state       =query-move-state
            intention   nil
        !output! ("---- 2.1.3a decide next action for disc, next move state is ~S" =query-move-state)
    )

    (p decide-next-action-rect
        =goal>
            state       deciding-next-action
            intention   test-intention
        ?imaginal>
            state free
        =imaginal>
            isa position-record
            is-rect 1
            rect-x =rx
            rect-y =ry
            rect-width =rw
            rect-height =rh
            diamond-x =diamx
            diamond-y =diamy
    ==>
        ;; !bind! =query-move-state (find-next-action-rect =rx =ry =ry =rw  =diamx =diamy)
        !bind! =query-move-state (find-next-action-rect =rx =ry =rw =rh 60 26)
        =imaginal>
        =goal>  
            state       =query-move-state
            intention   nil
        !output! ("---- 2.1.3b decide next action for rect, next move state is ~S" =query-move-state)
    )

    (p perform-move-right
        =goal>
            ;; state       dummy-moving-right-up
            state       move-right
            ;; intention   nil
            intention   =hook-intention
        ?manual>
            state free
    ==>
        +manual>
            cmd press-key
            key d
            duration 0.2
        =goal>
            ;; state    move-right
            intention   =hook-intention
            sub-intention nil
        !output! ("---- 3.1.1 Moving right with 'd'")
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
)