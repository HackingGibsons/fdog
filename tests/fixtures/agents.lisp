(in-package :afdog-tests)

;; Agents
(defclass test-agent (standard-agent)
  ()
  (:documentation "A `standard-agent' derivative we can insert probes into to test things."))

(defclass runner-agent (standard-agent)
  ()
  (:documentation "A `standard-agent' derivative we can insert probes into to test things."))

(defclass leaf-test-agent (agent::standard-leaf-agent)
  ()
  (:documentation "A `standard-leaf-agent' that can be instrumented."))

(defclass hypervisor-test-agent (agent::standard-hypervisor-agent)
  ()
  (:documentation "A `standard-hypervisor-agent' that I can instrument."))

(defmethod agent-special-event :after ((agent hypervisor-test-agent) (event-head (eql :boot)) event)
  ;; Boot the hypervisor and make it loud
  (make-announce-what-i-see (agent::find-organ agent :head))
  (make-announce-what-i-make (agent::find-organ agent :head))
  (make-make-agent-when-asked (agent::find-organ agent :head))
  (make-look-at-child-when-asked (agent::find-organ agent :head)))

(defmethod agent-special-event :after ((agent runner-agent) (head (eql :boot)) event)
  (make-speak-test-message (agent::find-organ agent :head))

  ;; Blatant test drivers
  (make-look-at-self-when-asked (agent::find-organ agent :head))
  (make-watch-self-when-asked (agent::find-organ agent :head))

  (make-announce-what-i-see (agent::find-organ agent :head)))


