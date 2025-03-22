;; Navigation functions for the GeoMates environment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Platform gap detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find_gap (rect-x rect-y rect-width rect-height)
  "Find platform gaps at the level of rect-bottom.
   Returns the left and right x-coordinates of the gap.
   Debug version: only handles specific cases with buffer of 2."
  (let* ((rect-bottom (- rect-y (/ rect-height 2)))
         (buffer 2))
    
    (cond
      ;; If rect-bottom is around 21 (with buffer of 2)
      ((and (>= rect-bottom (- 21 buffer))
            (<= rect-bottom (+ 21 buffer)))
       (values 30 40))
      
      ;; If rect-bottom is around 1 (with buffer of 2)
      ((and (>= rect-bottom (- 1 buffer))
            (<= rect-bottom (+ 1 buffer)))
       (values 0 0))
      
      ;; Default case
      (t (values 0 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation decision functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-next-action-disc-queue (diamond-x diamond-y disc-x disc-y rect-x rect-y rect-width rect-height)
  "Determine the next action for the disc to move toward the diamond.
   Returns one of: 'w:move-up, 's:move-down, 'a:move-left, 'd:move-right, 'right-high-jump, 'left-high-jump, or nil if at the target.
   Note: actions are string could be WD, wwww etc, which means the agent will keep moving
   WD means the agent will move up and right later
   wwww means the agent will keep moving up by 4 times
   "
  
  (let* (
         (disc-radius 1)
         (buffer-distance 3)
         (rect-left (- rect-x (/ rect-width 2)))
         (rect-right (+ rect-x (/ rect-width 2)))
         (rect-top (+ rect-y (/ rect-height 2)))
         (target-x diamond-x) ;; Set target-x to diamond-x as first step
         (target-y diamond-y) ;; Set target-y to diamond-y as first step
        )
    
    ;; Check if rectangle is between disc and diamond
    (cond
      ;; If disc is left of diamond (disc-x < diamond-x)
      ((and (< disc-x diamond-x) 
            (> rect-x disc-x) 
            (< rect-x diamond-x)
            (< disc-y rect-top))  ; Only if disc is below rect-top
       ;; If rect is right of disc (which it is in this condition)
       (setq target-x (- rect-left 2)
             target-y (+ rect-top 2)))
            
      ;; If disc is right of diamond (disc-x > diamond-x)
      ((and (> disc-x diamond-x) 
            (< rect-x disc-x) 
            (> rect-x diamond-x)
            (< disc-y rect-top))  ; Only if disc is below rect-top
       ;; If rect is left of disc (which it is in this condition)
       (setq target-x (+ rect-right 2)
             target-y (+ rect-top 2)))
             
      ;; If disc is above rect-top, target remains diamond
      ((> disc-y rect-top)
       (setq target-x diamond-x
             target-y diamond-y)))

    (cond

      ;; todo :
      ;; 1. if disc and diamond are some platfrom, then do
      ;; 2. is disc is above/under diamond platfrom, then do move to the platform
      ((and (< disc-y target-y)
            (< (abs (- disc-x target-x)) buffer-distance))
       (cond ((< disc-x target-x) "wd")
             ((> disc-x target-x) "ad") 
             (t "w")))

      ;; Regular horizontal movement
      ((< disc-x target-x) "d")
      ((> disc-x target-x) "a")
      
      ;; Default case
      ("w"))))

(defun find-next-action-rect-queue (rect-x rect-y rect-width rect-height diamond-x diamond-y)
  "Determine the next action for the rectangle to move toward the diamond."
  (let* ((rect-right (+ rect-x (/ rect-width 2)))
         (rect-left (- rect-x (/ rect-width 2)))
         (rect-top (+ rect-y (/ rect-height 2)))
        ;;  (rect-bottom (- rect-y (/ rect-height 2)))
         (stable-width (/ rect-width 4)))
    
    (multiple-value-bind (platform-gap-x-left platform-gap-x-right)
        (find_gap rect-x rect-y rect-width rect-height)
    
      (let ((gap-size (- platform-gap-x-right platform-gap-x-left))
            (gap-center (/ (+ platform-gap-x-left platform-gap-x-right) 2)))
        (cond
          ;; NEW: Check if diamond is below and try to use gap
          ((and (< diamond-y rect-y)                    ; Diamond is below
                (> gap-size 0)                          ; Gap exists
                ) 
           (cond ((< (abs (- rect-x gap-center)) 4) "wwwwww")
                ((< rect-x gap-center) "dddddd")   ; Move right towards gap
                (t "aaa")))                     ; Move left towards gap
                ;; note: ddddddd vs aaa is by purpose, to preovent rect is block rotate at the gap
          
          ;; Your existing conditions remain unchanged
          ((and (= rect-x diamond-x) (= rect-y diamond-y)) nil)
          
          ((< rect-right diamond-x) 
           (if (> rect-right platform-gap-x-right)
               "dddd"
               (if (and (> gap-size 0) (> gap-size stable-width))
                   "ssss"
                   "dddd")))
          
          ((> rect-left diamond-x) 
           (if (< rect-left platform-gap-x-left)
               "aaaa"
               (if (and (> gap-size 0) (> gap-size stable-width))
                   "ssss"
                   "aaaa")))
          
          ((< rect-top diamond-y) "wwww")
          (t "s"))))))


;; todo : add tempral buffer for the action
;; ex: .  waiting 0.5 second, then do action, and 1-9 is 1-9 seconds
;;    
(defun convert-wasd-to-move (action)
            "Convert WASD string to move action symbols"
            (cond ((string= action "w") 'move-up)
                  ((string= action "s") 'move-down) 
                  ((string= action "a") 'move-left)
                  ((string= action "d") 'move-right)
                  (t nil)))          


(defun get-query-move-type (next-action-queue)
  "Determine the next intention based on action queue length.
   Returns 'queue-query-move if queue has items, 'reload-query-move otherwise."
  (if (>= (length next-action-queue) 1)
      'queue-query-move
      'reload-query-move))                  