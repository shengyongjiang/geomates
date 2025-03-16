;; Navigation functions for the GeoMates environment

;; Function to determine the next action for the disc based on its position relative to the diamond
(defun find-next-action-disc (disc-x disc-y diamond-x diamond-y)
  "Determine the next action for the disc to move toward the diamond.
   Returns one of: 'move-up, 'move-down, 'move-left, 'move-right, or nil if at the target."
  
  ;; Debug code: always return 'up-right regardless of positions
  ;; 'dummy-moving-right-up
  ;; 'move-right
  
  ;; Original logic (commented out for debugging)
  (cond
    ((and (= disc-x diamond-x) (= disc-y diamond-y))      nil)
    
    ;; Prioritize horizontal movement first
    ((< disc-x diamond-x) 'move-right)
    ((> disc-x diamond-x) 'move-left)
    
    ;; ((< disc-y diamond-y) 'move-down)
    ((> disc-y diamond-y) 'move-up)
    
    (t nil))
  )

;; Function to determine the next action for the rectangle based on its position relative to the diamond
(defun find-next-action-rect (rect-x rect-y rect-width rect-height diamond-x diamond-y)
  "Determine the next action for the rectangle to move toward the diamond.
   Returns one of: 'up, 'down, 'left, 'right, or nil if at the target."
  ;; 'dummy-moving-right-up
  ;; 'move-right

  ; Calculate the right edge of the rectangle
  (let ((rect-right (+ rect-x (/ rect-width 2))))) 

  (cond
    ((and (= rect-x diamond-x) (= rect-y diamond-y))     nil)
    
    ;; ((< rect-y diamond-y) 'move-down)
    ;; ((> rect-y diamond-y) 'move-up)
    
    ((< rect-right diamond-x) 'move-right)
    ;; ((> rect-x diamond-x) 'move-left)
    (t 'move-up)
    ;; (t nil)
  )
)