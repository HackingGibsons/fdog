(in-package :afdog-tests)

;; Agents
(defclass test-agent (standard-agent)
  ()
  (:documentation "A `standard-agent' derivative we can insert probes into to test things."))

(defclass runner-agent (standard-agent)
  ()
  (:documentation "A `standard-agent' derivative we can insert probes into to test things."))

(defmethod agent-special-event :after ((agent runner-agent) (head (eql :boot)) event)
  (make-speak-test-message (agent::find-organ agent :head))

  ;; Blatant test drivers
  (make-look-at-self-when-asked (agent::find-organ agent :head))
  (make-watch-self-when-asked (agent::find-organ agent :head))

  (make-announce-what-i-see (agent::find-organ agent :head)))

