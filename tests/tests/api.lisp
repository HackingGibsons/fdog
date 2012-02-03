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

(def-test (api-agent-announces-provides :group api-agent-tests)
    (:equalp "api")
  (with-agent-conversation (m e) api-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (info (getf msg :info)
                (getf msg :info))
          (provides (getf info :provides)
                    (getf info :provides)))
         (provides (getf provides :request-processing)))))
