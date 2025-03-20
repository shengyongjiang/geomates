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

(defun find-next-action-disc (diamond-x diamond-y disc-x disc-y rect-x rect-y rect-width rect-height)
  "Determine the next action for the disc to move toward the diamond.
   Returns one of: 'move-up, 'move-down, 'move-left, 'move-right, or nil if at the target."
  
  ;; Define disc radius and buffer as constants
  (let ((disc-radius 1)
        (buffer-distance 0.5))
    
    ;; Original logic (commented out for debugging)
    (cond
      ;; If at the target, return nil
      ((and (= disc-x diamond-x) (= disc-y diamond-y)) nil)
      
      ;; ((and rect-x (< (+ disc-x disc-radius buffer-distance) rect-x)) 'move-up)
      ((or (> disc-x diamond-x) (< (abs (- disc-x diamond-x)) buffer-distance)) 'move-up)
      
      ((< disc-x diamond-x) 'move-right)
      ((> disc-x diamond-x) 'move-left)
      
      ;; ((< disc-y diamond-y) 'move-down)
      (t nil))))

(defun find-next-action-rect (rect-x rect-y rect-width rect-height diamond-x diamond-y)
  "Determine the next action for the rectangle to move toward the diamond with safety checks.
   Returns one of: 'move-up, 'move-down, 'move-left, 'move-right, or nil if at target."
  (let* ((rect-right (+ rect-x (/ rect-width 2)))
         (rect-left (- rect-x (/ rect-width 2)))
         (rect-top (+ rect-y (/ rect-height 2)))
         (rect-bottom (- rect-y (/ rect-height 2)))
         (half-width (/ rect-width 4)))
    
    (multiple-value-bind (platform-gap-x-left platform-gap-x-right)
        (find_gap rect-x rect-y rect-width rect-height)
    
      (let ((gap-size (- platform-gap-x-right platform-gap-x-left)))
        (cond
          ;; At target
          ((and (= rect-x diamond-x) (= rect-y diamond-y)) nil)
          
          ;; Horizontal movement to align with diamond
          ((< rect-right diamond-x) 
           (if (> rect-right platform-gap-x-right)
               'move-right
               ;; If there's a gap and it's too wide, move down to increase width
               (if (and (> gap-size 0) (> gap-size half-width))
                   'move-down
                   'move-right)))
          
          ((> rect-left diamond-x) 
           (if (< rect-left platform-gap-x-left)
               'move-left
               ;; If there's a gap and it's too wide, move down to increase width
               (if (and (> gap-size 0) (> gap-size half-width))
                   'move-down
                   'move-left)))
          
          ;; Vertical movement if horizontally aligned
          ((< rect-top diamond-y) 'move-up)
          (t nil))))))

