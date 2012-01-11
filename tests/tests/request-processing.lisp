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

(def-test (request-processing-agent-sees-mongrel2-agent :group request-processing-agent-tests :fixtures (db-path-fixture  mongrel2-agent-fixture request-processing-agent-fixture kill-everything-fixture))
    (:seq
     (:eql :informed-peers)
     (:eql :rp-agent-has-peers)
     (:eql :m2-agent-has-peers))
  (list
   (and (tell-agent-about request-processing-uuid mongrel2-uuid)
        :informed-peers)

   (with-agent-conversation (m e :timeout 35) request-processing-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (info (getf msg :info) (getf msg :info))
           (peers (getf info :peers) (getf info :peers)))
          (peers
           :rp-agent-has-peers)))

   (with-agent-conversation (m e :timeout 35) mongrel2-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (info (getf msg :info) (getf msg :info))
           (peers (getf info :peers) (getf info :peers)))
          (peers
           :m2-agent-has-peers)))))
