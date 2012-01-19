(in-package :forwarder-agent)


(defmethod agent-needs ((agent forwarder-agent) (organ agent-head) (what (eql :forwarder)) need-info)
  "Creates or updates a forwarder (\"forwarder server\" + named mongrel2 handler) in response to a need request."
  (labels ((from-info (thing) (getf need-info thing)))
    (let (forwarder-info (from-info forwarder))
      (labels ((from-forwarder (thing) (getf forwarder-info thing)))
        (let ((name (from-forwarder name)))
          ;; Announce "need forwarder server"
          (send-message organ :command
                        `(:command :speak
                          :say (:agent :need
                                       :need :server
                                       :server (:name "forwarder" :port ???? :hosts ("????")))))
          ;; Announce "need handler" for hostpath
          (send-message organ :command
                        `(:command :speak
                          :say (:agent :need
                                       :need :handler
                                       :handler (:server "forwarder" :port ???? :hosts ("????")))))
          ;; TODO: What if multiple hostpaths?
          ;; Then announce "need filled for forwarder"
          (send-message organ :command
                        `(:command :speak
                          :say (:filled :need
                                        :need ,what
                                        ,what ,need-info)))
          ;; Add forwarder to agent list
          (add-forwarder agent name)
          ;; TODO persistence
  )))))

(defmethod agent-needs ((agent forwarder-agent) (organ agent-head) (what (eql :remove-forwarders)) need-info)
  "Removes the named forwarders."
  ;; TODO announce handler removal
  ;; TODO persistence
  )
(defmethod agent-needs ((agent forwarder-agent) (organ agent-head) (what (eql :keep-forwarders)) need-info)
  "Removes all forwarders except those named."
  ;; TODO announce handler cull
  ;; TODO persistence
  )

(defmethod heard-message ((agent forwarder-agent) (organ agent-head) (from (eql :agent)) (type (eql :need)) &rest request)
  (log-for (trace forwarder-agent) "Heard an :agent :need message: ~A" request)
  (let* ((need-what (getf request :need))
         (need-info (getf request need-what)))
    (agent-needs agent organ need-what need-info)))
