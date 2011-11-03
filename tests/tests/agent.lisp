(in-package :afdog-tests)

(defmethod next-event :after ((agent test-agent))
  (case (agent-event-count agent)
    ;; Run the event tests
    (5 (nst:nst-cmd :run-group running-with-events-tests))

    ;; Remove an organ to test death from system failure
    ;; should race a timeout to fail
    (6 (let ((organ (find-organ agent :appendix)))
         (zmq:close (organ-incoming-sock organ))
         (zmq:close (organ-outgoing-sock organ))
         (setf (organ-incoming-sock organ) nil
               (organ-outgoing-sock organ) nil)
         (setf (agent-organs agent)
               (remove :appendix (agent-organs agent) :key #'organ-tag))))))

;; Tests to run after some events fire
(def-test (running-agent-has-events :group running-with-events-tests)
    (:predicate (lambda (v) (> v 0)))
  (agent-event-count agent))


;; Test spawning an agent
(def-test (agent-starts :group runner-tests) :true
  (running-p agent-runner))

(def-test (agent-stops :group runner-tests)
    (:process (:eval (stop agent-runner))
              (:check (:not (:true-form (running-p agent-runner))))))

