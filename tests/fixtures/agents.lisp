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
  (:documentation "A `afdog-hypervisor-test-agent' for testing the control of mongrel2 servers."))

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

(defmethod agent-special-event :after ((agent runner-agent) (head (eql :boot)) event)
  (make-speak-test-message (find-organ agent :head))

  ;; Blatant test drivers
  (make-look-at-self-when-asked (find-organ agent :head))
  (make-look-at-directory-when-asked (find-organ agent :head))
  (make-watch-self-when-asked (find-organ agent :head))

  (make-announce-what-i-see (find-organ agent :head))
  (make-kill-self-after-timeout (find-organ agent :head))
  (make-forge-agent-info (find-organ agent :head)))
