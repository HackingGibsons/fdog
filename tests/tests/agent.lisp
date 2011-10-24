(in-package :afdog-tests)

(defmethod next-event :after ((agent test-agent))
  (case (agent::agent-event-count agent)
    ;; Run the event tests
    (5 (nst:nst-cmd :run-group running-with-events-tests))

    ;; Remove an organ to test death from system failure
    ;; should race a timeout to fail
    (6 (let ((organ (agent::find-organ agent :appendix)))
         (zmq:close (agent::organ-incoming-sock organ))
         (zmq:close (agent::organ-outgoing-sock organ))
         (setf (agent::organ-incoming-sock organ) nil
               (agent::organ-outgoing-sock organ) nil)
         (setf (agent::agent-organs agent)
               (remove :appendix (agent::agent-organs agent) :key #'agent::organ-tag))))))

;; Tests to run after some events fire
(def-test (running-agent-has-events :group running-with-events-tests)
    (:predicate (lambda (v) (> v 0)))
  (agent-event-count agent))


;; Test spawning an agent
(def-test (agent-starts :group runner-tests) :true
  (agent::running-p agent-runner))

(def-test (agent-stops :group runner-tests)
    (:process (:eval (agent::stop agent-runner))
              (:check (:not (:true-form (agent::running-p agent-runner))))))

