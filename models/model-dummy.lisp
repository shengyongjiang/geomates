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

  (chunk-type goal state intention)
  (chunk-type control intention button)

    ;; phase 0: initial status
    ;; phase 1: found yellow disc
    ;; phase 2: found red block
    ;; phase 3: moving left
    (chunk-type position-record disc-x disc-y rect-x rect-y phase)

  (add-dm
   (move-left) (move-right)
   (move-up)  (move-down)
   (w) (a) (s) (d)
   (i-dont-know-where-to-go)
   (something-should-change)
   (i-want-to-do-something)
   (i-dont-know-who-i-am)
   (up-control isa control intention move-up button w)
   (down-control isa control intention move-down button s)
   (left-control isa control intention move-left button a)
   (right-control isa control intention move-right button d)
    (position-record-chunk isa position-record disc-x nil disc-y nil rect-x nil rect-y nil phase 0)
   (first-goal isa goal state i-dont-know-who-i-am)
   ;; (first-goal isa goal state i-dont-know-where-to-go)
   )

  (goal-focus first-goal)
  
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
    !output! ("---- 1.1 Searching for yellow disc with specific criteria")
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
    !output! ("---- 1.1a Search failed, trying again")
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
==>
    +visual>
        cmd move-attention
        screen-pos =visual-location
    +imaginal>
        isa position-record
        disc-x =x
        disc-y =y
        rect-x nil
        rect-y nil
        phase 1
    =goal>
        state ready-to-find-red-block
    !output! ("---- 1.2 Moving attention to object at x: ~S y: ~S" =x =y)
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
    !output! ("---- 2.1 Searching for red block with specific criteria")
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
    !output! ("---- 2.1a Search failed, trying again")
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
        isa position-record
        disc-x =disc-x
        disc-y =disc-y
==>
    +visual>
        cmd move-attention
        screen-pos =visual-location
    +imaginal>
        isa position-record
        disc-x =disc-x
        disc-y =disc-y
        rect-x =x
        rect-y =y
        phase 2
    =goal>
        state ready-to-move-left
    !output! ("---- 2.2 Moving attention to object at x: ~S y: ~S" =x =y)
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
