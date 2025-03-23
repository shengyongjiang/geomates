(load "actr7.y/load-act-r.lisp")
(load "geomates/act-r-experiment.lisp")

;; (load-act-r-model "code/DeepSeekAgent.lisp")

(load "code/navigation-functions.lisp")
(load-act-r-model "code/model-deepseek-agent1.lisp")

(run-environment)  ;; must enable this to run two agents in the same time machine
(geomates-experiment)