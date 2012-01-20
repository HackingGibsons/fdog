(in-package :forwarder-agent)


(defmethod agent-needs ((agent forwarder-agent) (organ agent-head) (what (eql :forwarder)) need-info)
  "Creates or updates a forwarder (\"forwarder server\" + named mongrel2 handler) in response to a need request."
  (labels ((from-info (thing) (getf need-info thing))
           (handler-name (name) (format nil "forwarder-~A" name)))
    (let ((name (from-info name))
          (hostpaths (from-info hostpaths)))
      ;; Announce "need forwarder server"
      (send-message organ :command
                    `(:command :speak
                               :say (:agent :need
                                            :need :server
                                            :server (:name "forwarder" :port ,*forwarder-server-port* :hosts ("localhost")))))
      ;; Announce "need handler" for hostpath
      ;; TODO currently only makes handler for first hostpath
      (send-message organ :command
                    `(:command :speak
                               :say (:agent :need
                                            :need :handler
                                            :handler (:server "forwarder" :hosts ,(caar hostpaths) :route ,(cdar hostpaths) :name ,(handler-name name)))))

      ;; TODO: What if multiple hostpaths?
      ;; Add forwarder to agent list
      (add-forwarder agent name)
      ;; TODO persistence

      ;; Then announce "need filled for forwarder"
      (send-message organ :command
                    `(:command :speak
                               :say (:filled :need
                                             :need ,what
                                             ,what ,need-info)))
  )))

(defmethod agent-needs ((agent forwarder-agent) (organ agent-head) (what (eql :remove-forwarders)) need-info)
  "Removes the named forwarders."
  (labels ((from-info (thing) (getf need-info thing)))
    (let ((names (from-info names)))
      (send-message organ :command
                    `(:command :speak
                               :say (:agent :need
                                            :need :remove-handlers
                                            :remove-handlers (:server "forwarder" :name ,names))))
      (remove-forwarders agent names))

    ;; announce handler removal
    (send-message organ :command
                  `(:command :speak
                             :say (:filled :need
                                           :need ,what
                                           ,what ,need-info)))
    ;; TODO persistence
  ))
(defmethod agent-needs ((agent forwarder-agent) (organ agent-head) (what (eql :keep-forwarders)) need-info)
  "Removes all forwarders except those named."
  (labels ((from-info (thing) (getf need-info thing)))
    (let ((names (from-info names)))
      (send-message organ :command
                    `(:command :speak
                               :say (:agent :need
                                            :need :keep-handlers
                                            :keep-handlers (:server "forwarder" :name ,names))))
      (cull-forwarders agent names))

    ;; announce handler removal
    (send-message organ :command
                  `(:command :speak
                             :say (:filled :need
                                           :need ,what
                                           ,what ,need-info)))
    ;; TODO persistence
  ))

(defmethod heard-message ((agent forwarder-agent) (organ agent-head) (from (eql :agent)) (type (eql :need)) &rest request)
  (log-for (trace forwarder-agent) "Heard an :agent :need message: ~A" request)
  (let* ((need-what (getf request :need))
         (need-info (getf request need-what)))
    (agent-needs agent organ need-what need-info)))
