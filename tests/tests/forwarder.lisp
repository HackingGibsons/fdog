(in-package :afdog-tests)

;;; Fixtures needed: mongrel2-agent, forwarder-agent

(def-test (forwarder-agent-create :group forwarder-agent-tests) (:eql :pending)
  ;;; Announce "need forwarder named foo"
  ;;; the handler gets created and announced
  ;;; check the handler exists
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
