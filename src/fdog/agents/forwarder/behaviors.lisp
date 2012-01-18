(in-package :forwarder-agent)


(defmethod agent-needs ((agent forwarder-agent) (organ agent-head) (what (eql :forwarder)) need-info)
  "Creates or updates a forwarder (\"forwarder server\" + named mongrel2 handler) in response to a need request."
  (labels ((from-info (thing) (getf need-info thing)))
    (let (forwarder-info (from-info forwarder))
      (labels ((from-forwarder (thing) (getf forwarder-info thing)))
  ;; Announce "need forwarder server" - with whatever that needs
  ;; Announce "need handler" for hostpath
  ;; What if multiple hostpaths?
  ;; Then announce "need filled for forwarder"
  ;; Add it to ??? for how the :provides works
  ;; (see mongrel2 agent
  ;; TODO persistence
  ))))

(defmethod agent-needs ((agent forwarder-agent) (organ agent-head) (what (eql :remove-forwarder)) need-info)
  "Removes the named forwarders."
  ;; TODO persistence
  )
(defmethod agent-needs ((agent forwarder-agent) (organ agent-head) (what (eql :keep-forwarders)) need-info)
  "Removes all forwarders except those named."
  ;; TODO persistence
  )
