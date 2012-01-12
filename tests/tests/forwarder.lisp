(in-package :afdog-tests)

;;; Fixtures needed: mongrel2-agent, forwarder-agent

(def-test (forwarder-agent-create :group forwarder-agent-tests) (:eql :pending)
  ;;; Announce "need forwarder named foo"
  ;;; the handler gets created and announced
  ;;; check the handler exists
  (list
   (with-agent-conversation (m e) agent-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need :forwarder
                            :forwarder ;; TODO
                            )))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :forwarder))
           (log-for (trace fdog-agent::agent-needs) "Filled: ~A" msg))))
   )
  :pending)

(def-test (forwarder-agent-update :group forwarder-agent-tests) (:eql :pending)
  ;;; Announce "need forwarder foo, new config: (different-config)"
  ;;; Handler gets updated, announced when done
  ;;; check the configs differ
  :pending)

(def-test (forwarder-agent-remove :group forwarder-agent-tests) (:eql :pending)
  ;;; Announce "need forwarder foo deleted"
  ;;; handler deleted, announced
  :pending)

(def-test (forwarder-agent-cull :group forwarder-agent-tests) (:eql :pending)
  ;;; Announce "need forwarder culling, save foo and bar"
  ;;; Other handlers deleted
  ;;; check foo and bar exist, but not baz
  :pending)
