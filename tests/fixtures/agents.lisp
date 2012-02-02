(in-package :afdog-tests)

;; Agents
;; TODO: Please make inheritence sense out of this mess :(
(defclass test-agent (standard-agent)
  ()
  (:documentation "A `standard-agent' derivative we can insert probes into to test things."))

(defclass runner-agent (standard-agent)
  ()
  (:documentation "A `standard-agent' derivative we can insert probes into to test things."))

(defclass leaf-test-agent (standard-leaf-agent)
  ()
  (:documentation "A `standard-leaf-agent' that can be instrumented."))

(defclass hypervisor-test-agent (standard-hypervisor-agent)
  ()
  (:documentation "A `standard-hypervisor-agent' that I can instrument."))

(defclass mongrel2-test-agent (mongrel2-agent)
  ()
  (:documentation "A `mongrel2-test-agent' for testing the control of mongrel2 servers."))

(defclass afdog-hypervisor-test-agent (afdog-hypervisor-agent)
  ()
  (:documentation "A `afdog-hypervisor-test-agent' for testing the control of mongrel2 servers.")
  (:default-initargs . (:agents '(mongrel2-test-agent ()))))

(defclass request-processing-test-agent (request-processing-agent)
  ()
  (:documentation "A `request-processing-agent` for testing the behavior of request processing."))

(defmethod agent-special-event :after ((agent hypervisor-test-agent) (event-head (eql :boot)) event)
  ;; Boot the hypervisor and make it loud
  (make-spawn-dependant-when-asked (agent::find-organ agent :head))
  (make-spawn-process-when-asked (find-organ agent :head))
  (make-link-to-process-when-asked (find-organ agent :head))
  (make-announce-what-i-see (find-organ agent :head))
  (make-announce-what-i-make (find-organ agent :head))
  (make-announce-what-i-unlink (find-organ agent :head))
  (make-announce-when-links-collide (find-organ agent :head))
  (make-make-agent-when-asked (find-organ agent :head))
  (make-make-process-when-asked (find-organ agent :head))
  (make-unlink-when-asked (find-organ agent :head))
  (make-look-at-child-when-asked (find-organ agent :head))
  (make-kill-self-after-timeout (find-organ agent :head)))

(defmethod agent-special-event :after ((agent mongrel2-test-agent) (event-head (eql :boot)) event)
  (make-announce-what-i-see (find-organ agent :head))
  (make-announce-what-i-make (find-organ agent :head))
  (make-kill-self-after-timeout (find-organ agent :head)))

(defmethod agent-special-event :after ((agent afdog-hypervisor-test-agent) (event-head (eql :boot)) event)
  (make-announce-what-i-see (find-organ agent :head))
  (make-announce-what-i-make (find-organ agent :head))
  (make-kill-self-after-timeout (find-organ agent :head)))

(defmethod agent-special-event :after ((agent request-processing-test-agent) (event-head (eql :boot)) event)
  (make-speak-request-processing-messages (find-organ agent :head))
  (make-kill-self-after-timeout (find-organ agent :head)))

(defmethod request-handler :before ((agent request-processing-test-agent) (organ agent-requesticle) msg)
  (log-for (trace request-processing-agent::request-handler) "Announcing request: ~Ab" (zmq:msg-size msg))
  (send-message organ :request-handler `(:request-handler :raw
                                         :raw ,(zmq:msg-data-string msg))))

(defmethod heard-message ((agent runner-agent) (organ agent::agent-head)
                          (from (eql :agent)) (type (eql :crash)) &rest request)
  (let ((who (getf request :agent)))
    (when (string-equal who (agent-uuid agent))
      (error "Asked to crash: Message: [~S]" request))))

(defmethod agent-special-event :after ((agent runner-agent) (head (eql :boot)) event)
  (make-speak-test-message (find-organ agent :head))

  ;; Blatant test drivers
  (make-look-at-self-when-asked (find-organ agent :head))
  (make-look-at-directory-when-asked (find-organ agent :head))
  (make-watch-self-when-asked (find-organ agent :head))

  (make-announce-what-i-see (find-organ agent :head))
  (make-kill-self-after-timeout (find-organ agent :head))
  (make-forge-agent-info (find-organ agent :head)))
