(in-package :afdog-tests)

(def-test (request-processing-agent-starts :group request-processing-agent-tests :fixtures (request-processing-agent-fixture kill-everything-fixture))
    (:seq (:eql :starts)
          (:eql :running))
  (list
   (with-agent-conversation (m e) request-processing-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (info (getf msg :info) (getf msg :info)))
          ((and info)
           :starts)))

   (and (running-p request-processing-runner) :running)))
