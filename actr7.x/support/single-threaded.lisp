;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2019 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : single-threaded.lisp
;;; Version     : 3.0
;;; 
;;; Description : No-op all the quicklisp loaded package actions.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2019.04.09 Dan
;;;             : * Created this to provide a fast single threaded version.
;;; 2019.04.17 Dan
;;;             : * Didn't add the acquire/release-recursive-lock functions.
;;; 2019.05.29 Dan [2.0]
;;;             : * Actually define the packages and all the Quicklisp library
;;;             :   functions used in the code so I can skip Quicklisp for the
;;;             :   single-threaded version.
;;; 2019.05.30 Dan
;;;             : * Remove the jsown package and stub.
;;; 2019.11.12 Dan
;;;             : * The stubs for acquire-lock and acquire-recursive-lock need
;;;             :   to return t otherwise it looks like a failure.
;;; 2024.07.02 Dan [3.0]
;;;             : * Only create the stubs if the packages don't already exist
;;;             :   by checking for the package names (I'd prefer a feature
;;;             :   test, but usocket doesn't seem to have one and don't want
;;;             :   to use different mechanisms based on package).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Allows the ACT-R code to compile and run properly while ignoring all of the
;;; locking actions and other Quicklisp library code.  This of course means that
;;; the code is no longer thread safe.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; None.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Just make the lock based actions no-ops by redefining the functions and
;;; macros from the :bordeaux-threads package with dummies.
;;;
;;; Create dummy stubs for all the other functions referenced in the code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;; Make sure not to start the dispatcher

(pushnew :standalone *features*)

;; define the packages which would come from Quicklisp loads
;; if they don't already exist.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :bordeaux-threads)
    (defpackage :bordeaux-threads
      (:nicknames :bt)
      (:use :cl)
      (:export
       :make-lock
       :acquire-lock
       :release-lock
       :with-lock-held
       :make-recursive-lock
       :acquire-recursive-lock
       :release-recursive-lock
       :with-recursive-lock-held
       :make-condition-variable
       :condition-notify
       :condition-wait
       :make-thread
       :threadp
       :thread-alive-p
       :destroy-thread
       :thread-yield))
    (pushnew :dummy-bordeaux *features*)))

#+:dummy-bordeaux
(progn
    (defmacro bordeaux-threads::with-lock-held ((place) &body body)
      (declare (ignore place))
      `(progn
         ,@body))
    
    (defmacro bordeaux-threads::with-recursive-lock-held ((place &key timeout) &body body)
      (declare (ignore place timeout))
      `(progn
         ,@body))
    
    (defun bordeaux-threads::make-lock (&optional x) (declare (ignore x)))
    (defun bordeaux-threads::make-recursive-lock (&optional x) (declare (ignore x)))
    (defun bordeaux-threads::acquire-lock (l &optional w) (declare (ignore l w)) t)
    (defun bordeaux-threads::release-lock (l) (declare (ignore l)))
    (defun bordeaux-threads::acquire-recursive-lock (l) (declare (ignore l)) t)
    (defun bordeaux-threads::release-recursive-lock (l) (declare (ignore l)))
    (defun bordeaux-threads::make-condition-variable (&rest x) (declare (ignore x)))
    (defun bordeaux-threads::condition-notify (&rest x) (declare (ignore x)))
    (defun bordeaux-threads::condition-wait (&rest x) (declare (ignore x)))
    
    (defun bordeaux-threads::make-thread (&rest x) (declare (ignore x)))
    (defun bordeaux-threads::threadp (&rest x) (declare (ignore x)))
    (defun bordeaux-threads::thread-alive-p (&rest x) (declare (ignore x)))
    (defun bordeaux-threads::destroy-thread (&rest x) (declare (ignore x)))
    (defun bordeaux-threads::thread-yield (&rest x) (declare (ignore x)))
    
    )


(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :usocket)
    
    (defpackage :usocket
      (:use :cl)
      (:export 
       :socket-stream
       :socket-close
       :socket-listen
       :socket-accept
       :ip=))
    (pushnew :dummy-usocket *features*)))

#+:dummy-usocket
(progn
 
    (defun usocket::socket-stream (&rest x) (declare (ignore x)))
    (defun usocket::socket-close (&rest x) (declare (ignore x)))
    (defun usocket::socket-listen (&rest x) (declare (ignore x)))
    (defun usocket::socket-accept (&rest x) (declare (ignore x)))
    (defun usocket::ip= (&rest x) (declare (ignore x)))
    
    (defun usocket::get-peer-port (&rest x) (declare (ignore x)))
    (defun usocket::get-peer-address (&rest x) (declare (ignore x)))
    (defun usocket::split-sequence (&rest x) (declare (ignore x)))
    (defun usocket::get-hosts-by-name (&rest x) (declare (ignore x)))
    (defun usocket::get-host-name (&rest x) (declare (ignore x)))
    (defun usocket::vector-quad-to-dotted-quad (&rest x) (declare (ignore x)))
    (defun usocket::usocket-p (&rest x) (declare (ignore x)))
    
    (define-condition usocket::address-in-use-error () ())
  )
    
    
    
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :json)
    
    (defpackage :json
      (:nicknames :cl-json)
      (:use :cl)
      (:export
   
       :*lisp-identifier-name-to-json*
       :encode-json-to-string
       :decode-json-from-string
       :decode-json-from-source))
    (pushnew :dummy-json *features*)))

#+:dummy-json
(progn
    (defparameter json::+json-lisp-symbol-tokens+ nil)
    (defvar json::*json-output* nil)
    (defvar json::*lisp-identifier-name-to-json* nil)

    (defun json::write-json-string (&rest x) (declare (ignore x)))
    (defun json::encode-json-to-string (s &rest x) (declare (ignore x)) s)
    (defun json::decode-json-from-string (s &rest x) (declare (ignore x)) s)
    (defun json::decode-json-from-source (s &rest x) (declare (ignore s x)))

    (defgeneric json::encode-json (object &optional stream))

    (defmethod json::encode-json (x &optional s)
      (declare (ignorable x s)))
    
  )

   

;; Switch back to the original package for loading ACT-R

;#+:packaged-actr (in-package :act-r)
;#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
;#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
