(in-package :agent)

(defclass agent-appendix (standard-organ) nil
  (:documentation "An organ that exists solely to exist. Built to scaffold the messaging architecture"))

(defmethod act-on-event ((appendix agent-appendix) event)
  (let ((parsed (call-next-method)))
    (log-for (trace) "Acting as ~A on event: ~A" appendix parsed)

    (cond ((eql (getf parsed :heart) :beat)
           (log-for (trace) "~A replying to heartbeat." appendix)
           (send-message appendix `(:appendix :beat :uuid ,(organ-uuid appendix) :time ,(get-internal-real-time))))
          (t
           (log-for (trace) "~A => ~A: Discarding" parsed appendix)))))
