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
   Returns one of: 'move-up, 'move-down, 'move-left, 'move-right, 'right-high-jump, 'left-high-jump, or nil if at the target."
  
  (let* (
         (disc-radius 1)
         (buffer-distance 0.5)
        ;;  (rect-left (- rect-x (/ rect-width 2)))
        ;;  (rect-right (+ rect-x (/ rect-width 2)))
        )
    
    (cond
      ;; If at the target, return nil
      ((< (abs (- disc-x diamond-x)) buffer-distance) "w")
      
      ;; High jump actions based on rectangle position
      ;; ((> rect-left disc-x) 'right-high-jump)
      ;; ((< rect-right disc-x) 'left-high-jump)
      
      ;; Regular horizontal movement
      ((< disc-x diamond-x) "dw")
      ((> disc-x diamond-x) "aw")
      
      ;; Vertical movement
      ;; ((< disc-y diamond-y) "s")
      ((> disc-y diamond-y) "w")
      
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
                ((< rect-x gap-center) "d")   ; Move right towards gap
                (t "a")))                     ; Move left towards gap
          
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