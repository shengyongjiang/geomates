;; Navigation functions for the GeoMates environment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Platform gap detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find_gap (rect-x rect-y rect-width rect-height)
  "Return hardcoded platform gap coordinates (left edge, right edge)"
  (values 30 40))

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
  "Determine the next action for the rectangle to move toward the diamond with safety checks.
   Returns one of: 'move-up, 'move-down, 'move-left, 'move-right, or nil if at target."
  (let* ((rect-right (+ rect-x (/ rect-width 2)))
         (rect-left (- rect-x (/ rect-width 2)))
         (rect-top (+ rect-y (/ rect-height 2)))
        ;;  (rect-bottom (- rect-y (/ rect-height 2)))
         (stable-width (/ rect-width 4)))
    
    (multiple-value-bind (platform-gap-x-left platform-gap-x-right)
        (find_gap rect-x rect-y rect-width rect-height)
    
      (let ((gap-size (- platform-gap-x-right platform-gap-x-left)))
        (cond
          ;; At target
          ((and (= rect-x diamond-x) (= rect-y diamond-y)) nil)
          
          ;; Horizontal movement to align with diamond
          ((< rect-right diamond-x) 
           (if (> rect-right platform-gap-x-right)
               "dddd"
               ;; If there's a gap and it's too wide, move down to increase width
               (if (and (> gap-size 0) (> gap-size stable-width))
                   "ssss"
                   "dddd")))
          
          ((> rect-left diamond-x) 
           (if (< rect-left platform-gap-x-left)
               "aaaa"
               ;; If there's a gap and it's too wide, move down to increase width
               (if (and (> gap-size 0) (> gap-size stable-width))
                   "ssss"
                   "aaaa")))
          
          ;; Vertical movement if horizontally aligned
          ((< rect-top diamond-y) "wwww")
          (t nil))))))


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