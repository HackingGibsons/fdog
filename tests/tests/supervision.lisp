(in-package :afdog-tests)

(def-test (hyperrunner-is-running :group supervision-tests) :true
  (agent::running-p agent-runner))

(def-test (hypervisor-is-speaking :group supervision-tests) :true
  (with-agent-conversation (mouth-sock ear-sock) agent-uuid
    (agent::parse-message (agent::read-message mouth-sock))))

(def-test (child-runner-exists :group supervision-tests :fixtures (running-hypervisor-child)) :true
  child-runner)

(def-test (child-runner-running :group supervision-tests :fixtures (running-hypervisor-child)) :true
  (agent::running-p child-runner))

(def-test (child-is-speaking :group supervision-tests :fixtures (running-hypervisor-child)) :true
  (with-agent-conversation (mouth-sock ear-sock) child-uuid
    (agent::parse-message (agent::read-message mouth-sock))))
