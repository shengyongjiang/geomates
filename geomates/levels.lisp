;;;
;;; Levels are defined in a global variable *levels* as list of levels. Each level consists out of objects
;;; given as s-expressions. The coordinate system is x to the right, positive y is up. Unit size is meter.
;;; Levels should be 80 x 40 in size to fit the graphical output. 
;;; Format and semantics are as follows:
;;; (:rect x y) starting position of the rect, the rect player starts with initial width according to +rect-length+ (2m)
;;; (:disc x y) starting position of the disc, the disc is of radius +disc-radius+ (1.0m)
;;; (:platform x1 y1 x2 y2) defines a rectangular platform with corners (x1 y1) and (x2 y2)
;;;                         be nice and add a ground plane and side walls to avoid your players falling off accidentally
;;; (:diamond x y) diamond to collect: each level must contain at least one diamand as a level is said to be finished if
;;;                no diamonds are left
;;; 

(defparameter *levels*
  '(;level 1
    ((:disc 10 23)
     (:rect 40 23)
     (:platform 0 0 80 1)
     (:platform 0 39 80 40)
     (:platform 0 1 1 40)
     (:platform 79 1 80 40)
     (:platform 1 20 30 21)
     (:platform 40 20 79 21)
     (:diamond 20 26)
     (:diamond 60 26)
     (:diamond 35 10))
    )
  "level description for simple test levels")
