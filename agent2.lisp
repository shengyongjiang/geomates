(load "actr7.y/load-act-r.lisp")
(load "geomates/act-r-experiment.lisp")

(load-act-r-model "code/DeepSeekAgent.lisp")

(setf *actr-instance-id* "AGENT2-INSTANCE")
(format t "~%Running with ACT-R instance ID: ~a~%" *actr-instance-id*)

(run-environment)  ;; must enable this to run two agents in the same time machine
(geomates-experiment)