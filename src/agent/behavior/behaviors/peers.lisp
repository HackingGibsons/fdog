(in-package :agent)

(defbehavior announce-self (:interval (:from :heart :nth 3) :do :invoke) (organ)
  (log-for (warn) "TODO: Running ~A behavior lambda for ~A" behavior organ)
  (send-message organ `(,(organ-tag organ) :command
                         :command :speak
                         :say (:agent :info :info ,(agent-info (organ-agent organ)))))
  (log-for (warn) "Message sent."))
