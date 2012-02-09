(in-package :afdog-tests)

(def-test (request-forwarding-agent-starts :group request-forwarder-agent-tests)
    (:values (:eql :starts)
             (:eql :running))
  (with-agent-conversation (m e) request-forwarder-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (info (getf msg :info) (getf msg :info)))
         ((and info)
          :starts)))

   (and (running-p request-forwarder-runner) :running))

(def-test (request-forwarder-agent-announces-provides :group request-forwarder-agent-tests)
    (:equalp "forwarder-test-default")
  (with-agent-conversation (m e) request-forwarder-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (info (getf msg :info)
                (getf msg :info))
          (provides (getf info :provides)
                    (getf info :provides)))
         (provides (getf provides :request-processing)))))
