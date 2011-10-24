(in-package :afdog-tests)

(def-test (hyperrunner-is-running :group supervision-tests) :true
  (agent::running-p agent-runner))
