(in-package :afdog-tests)

(def-test (api-agent-starts :group api-agent-tests)
    (:seq (:eql :starts)
          (:eql :running))
  (list
   (with-agent-conversation (m e) api-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (info (getf msg :info) (getf msg :info)))
          ((and info)
           :starts)))

   (and (running-p api-runner) :running)))
