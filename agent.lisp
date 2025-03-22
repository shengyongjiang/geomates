(load "actr7.x/load-act-r.lisp")
(load "geomates/act-r-experiment.lisp")
;; (load "geomates/levels.lisp")

(load "code/navigation-functions.lisp")

(load-act-r-model "code/model-shengyong-jiang-agent.lisp")

(setf *actr-instance-id* "AGENT-INSTANCE")
(format t "~%Running with ACT-R instance ID: ~a~%" *actr-instance-id*)

(run-environment)  ;; must enable this to run two agents in the same time machine
(geomates-experiment)