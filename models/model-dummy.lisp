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
  (sgp :imaginal-activation t                   ;; Store imaginal buffer contents in DM
       :time-master-start-increment 60.0)       ;; Set time limit to 60 seconds

  (chunk-type goal state intention)
  (chunk-type control intention button)

    ;; who-i-am: the name of the agent, could be "yellow-disc" or "red-block"
    ;; phase 0: initial status
    ;; phase 10: found yellow disc
    ;; phase 20: found red block
    ;; phase 30: moving left
    ;; phase 40: found yellow disc after moving
    ;; phase 50: retrieved previous position record and compared with current position

    ;; disc speed: 0 not moving, 1 moving right once 2 move right twice etc, and - is moving left
    ;; block expand: 0 not expand, 1 expand  once 2 expand twice etc
    (chunk-type position-record 
                                is-disc  speed disc-x disc-y 
                                is-block expand rect-x rect-y who-i-am phase
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
   (red-block)
   (up-control isa control intention move-up button w)
   (down-control isa control intention move-down button s)
   (left-control isa control intention move-left button a)
   (right-control isa control intention move-right button d)
   (position-record-chunk isa position-record is-disc 0 speed 0 disc-x nil disc-y nil is-block 0 rect-x nil rect-y nil phase 0)
   (first-goal isa goal state i-dont-know-who-i-am)
   (second-goal isa goal state searching-for-diamond)
   (third-goal isa goal state random-moving-right-jump intention test-initialize)
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
        state searching-for-yellow-disc
    !output! ("---- 1.1.1 Searching for yellow disc with specific criteria")
)

;; Simple production to loop back when search fails
(p retry-find-yellow-disc
    =goal>
        state searching-for-yellow-disc
    ?visual-location>
        buffer failure
    ?visual>
        state free
==>
    =goal>
        state i-dont-know-who-i-am
    !output! ("---- 1.1.2 Search failed, trying again")
)

;; Only proceed to attend when we have a visual location
(p attend-to-yellow-disc
    =goal>
        state searching-for-yellow-disc
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
        state ready-to-find-red-block
    !output! ("---- 1.2.1 Moving attention to object at x: ~S y: ~S" =x =y)
)


  
;; Step 1: Record the initial position of the yellow disc
(p find-red-block
    =goal>
        state ready-to-find-red-block
    ?visual>
        state free
    ?imaginal>
        state free
==>
    +visual-location>
        value       "rect"
        :attended   nil
    =goal>
        state searching-for-red-block
    !output! ("---- 1.2.2 Searching for red block with specific criteria")
)

(p retry-find-red-block
    =goal>
        state searching-for-red-block
    ?visual-location>
        buffer failure
    ?visual>
        state free
==>
    =goal>
        state ready-to-find-red-block
    !output! ("---- 1.2.3 Search failed, trying again")
)

;; Only proceed to attend when we have a visual location
(p attend-to-red-block
    =goal>
        state searching-for-red-block
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
        state ready-to-move-right
    !output! ("---- 1.2.4 Moving attention to object at x: ~S y: ~S" =x =y)
)

(p move-right-to-location-continue
    =goal>
        state ready-to-move-right
    =imaginal>
        phase  =phase
        - phase  33
    ?manual>
        state free
    !bind! =next-phase (+ =phase 1)
==>
    =goal>
        state ready-to-move-right
    +manual>
        cmd press-key
        key d
        duration 0.5
    =imaginal>
        isa position-record
        phase  =next-phase
    !output! ("---- 1.2.5 Moving right, phase ~S -> ~S" =phase =next-phase)
)

(p move-right-to-location-end
    =goal>
        state ready-to-move-right
    =imaginal>
        phase  =phase
        > phase  31
    ?manual>
        state free
==>
    =goal>
        state ready-re-find-yellow-disc
    +manual>
        cmd press-key
        key d
        duration 0.2
    =imaginal>
        isa position-record
        phase  40
    !output! ("---- 1.2.6 Moving right, phase ~S(ending loop)" =phase)
)

(p ready-re-find-yellow-disc
    =goal>
        state ready-re-find-yellow-disc
    ?visual>
        state free
==>
    +visual-location>
        value "disc"
        ;; :attended nil         ;; Look for unattended disc
    =goal>
        state searching-for-yellow-disc-after-move
    !output! ("---- 1.3.1 Re-Find yellow disc after moving")
)

;; Handle the case where we can't find the disc after moving
(p disc-search-failure-after-move
    =goal>
        state searching-for-yellow-disc-after-move
    ?visual-location>
        buffer failure
==>
    =goal>
        state ready-re-find-yellow-disc
    !output! ("---- 1.3.2 Failed to find disc after moving")
)

;; New productions to find and compare disc positions after moving

;; Find the new location of the disc after moving
(p re-attend-to-yellow-disc
    =goal>
        state searching-for-yellow-disc-after-move
    =visual-location>
        screen-x =new-disc-x
    =imaginal>
        isa position-record
        disc-x =old-disc-x
    ?visual>
        state free
==>
    +visual>
        cmd move-attention  
        screen-pos =visual-location
    =goal>
        state ready-re-find-red-block
    !bind! =is-disc (if (eql =new-disc-x =old-disc-x) 0 1)
    =imaginal>
        isa position-record
        disc-x =new-disc-x
        is-disc =is-disc
        phase 40
    !output! ("---- 1.3.3 Found disc at new position: x=~S(am I disc?: ~S)" =new-disc-x =is-disc)
)

(p ready-re-find-red-block
    =goal>
        state ready-re-find-red-block
    ?visual>
        state free
==>
    +visual-location>
        value "rect"
        ;; :attended nil         ;; Look for unattended rect
    =goal>
        state searching-for-red-block-after-move
    !output! ("---- 1.3.4 Re-Find red block after moving")
)

(p re-attend-to-red-block
    =goal>
        state searching-for-red-block-after-move
    =visual-location>
        screen-x =new-rect-x
    =imaginal>
        isa position-record
        rect-x =old-rect-x
    ?visual>
        state free
==>
    +visual>
        cmd move-attention  
        screen-pos =visual-location
    =goal>
        ;; state searching-for-diamond
        state random-moving-right-jump
        intention move-right
    !bind! =is-block (if (eql =new-rect-x =old-rect-x) 0 1)
    =imaginal>
        isa position-record
        rect-x =new-rect-x
        is-block =is-block
        phase 50
    !output! ("---- 1.3.5 Found red block at new position: x=~S(am I block?: ~S)" =new-rect-x =is-block)
)

;; Step 2 Find the nearest diamond from the disc
;; (p find-nearest-diamond
;;     =goal>
;;         state searching-for-diamond
;;     =imaginal>
;;         isa position-record
;;         disc-x =disc-x
;;         is-disc =1
;;     ?visual>
;;         state free
;; ==>
;;     +visual-location>
;;         value "diamond"
;;         screen-x closest =disc-x
;;     =goal>
;;         state evaluating-diamond
;;     !output! ("---- 2.1.1 Finding nearest diamond from disc at x=~S" =disc-x)
;; )


;; Step 3 random moving right jump
;; todo  only for testing
;; (p initialize-imaginal
;;     =goal>
;;         state random-moving-right-jump
;;         intention test-initialize
;;     ?imaginal>
;;         state free
;;         buffer empty
;; ==>
;;     =goal>
;;         state random-moving-right-jump
;;         intention move-right
;;     +imaginal>
;;         isa position-record
;;         speed 0
;;     !output! ("---- 3.0.1 Initializing imaginal buffer with starting values")
;; )

(p perform-move-right
    =goal>
        state random-moving-right-jump
        intention move-right
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
        state random-moving-right-jump
        intention move-right
    !output! ("---- 3.1.1 Moving right with 'd' (move ~S of 2)" =new-speed)
)

(p ready-for-jump-after-move-right
    =goal>
        state random-moving-right-jump
        intention move-right
    =imaginal>
        isa position-record
        ;; is-disc =1
        ;; is-rect =1
        speed   =current-speed
        > current-speed 3
    ?manual>
        state free
==>
    =goal>
        state random-moving-right-jump
        intention jump
    =imaginal>
    !output! ("---- 3.1.2 Ready for jump after moving right")
)

(p perform-jump
    =goal>
        state random-moving-right-jump
        intention jump
    =imaginal>
        isa position-record
        ;; is-disc =1
        ;; is-rect =1
    ?manual>
        state free
==>
    =goal>
        state random-moving-right-jump
        intention jump-key-pressed
    =imaginal>
    +manual>
        cmd press-key
        key w
        duration 0.2
    !output! ("---- 3.1.3 Performing jump with 'w' after moving right")
)

(p complete-action
    =goal>
        state random-moving-right-jump
        intention jump-key-pressed
    =imaginal>
        ;; isa position-record
        ;; is-disc =1
        ;; is-rect =1
    ?manual>
        state free
==>
    =goal>
        state random-moving-right-jump
        intention move-right
    =imaginal>
        speed 0
    !output! ("---- 3.1.4 Action completed, ready to jump or move right")
)

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


;; ???? 
;; Handle retrieval failure
(p retrieval-failure-position
    =goal>
        state waiting-for-retrieval
    ?retrieval>
        buffer failure
==>
    =goal>
        state retrieval-failed
    !output! ("---- 3.5 Failed to retrieve previous position record")
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
