(in-package :afdog-tests)

;; Test cases
(def-test (can-test-nothing :group basic-tests) :true
  t)

(def-test (fresh-agent-is-fresh :group basic-tests :fixtures (agent-fixture))
    (:all (:apply agent-event-count (:predicate zerop))
          (:apply agent-context (:not :true)))
  agent)

(def-test (spawner-is-test :group basic-tests :fixtures (spawner-fixture))
    (:eql :test)
  afdog-tests::*spawner*)

(def-test (state-machine-fires-boot-event-and-be-in-initial-state :group basic-tests :fixtures (test-state-machine-fixture))
    (:seq :true (:eql :initial))
  (list (test-machine-booted test-machine) (state test-machine)))

