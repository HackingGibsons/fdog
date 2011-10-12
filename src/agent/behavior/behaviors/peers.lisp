(in-package :agent)

(defbehavior announce-self (:interval (:from :heart :nth 3) :do :invoke) (organ)
  (log-for (trace) "Running ~A behavior lambda for ~A" behavior organ)
  (send-message organ `(,(organ-tag organ) :command
                         :command :speak
                         :say (:agent :info :info ,(agent-info (organ-agent organ)))))
  (log-for (trace) "Message sent."))

(defbehavior speak-when-told (:on (:command :speak :from :head) :do :invoke-with-event) (organ event)
  (let ((message (getf event :say)))
    (send-message organ message :sock (speak-sock organ))
    (log-for (trace) "~A => ~A has spoken: ~A" organ (speak-addr organ) message)))
