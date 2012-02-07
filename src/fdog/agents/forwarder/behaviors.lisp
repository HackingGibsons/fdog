(in-package :forwarder-agent)


(defmethod agent-needs ((agent forwarder-agent) (organ agent-head) (what (eql :forwarder)) need-info)
  "Creates or updates a forwarder (\"forwarder server\" + named mongrel2 handler) in response to a need request."
  (labels ((from-info (thing) (getf need-info thing))
           (handler-name (name route) (format nil "forwarder-~A-~A" name route)))
    (let ((name (from-info :name))
          (hosts (from-info :hosts))
          (routes (from-info :routes)))
      ;; Announce "need forwarder server"
      (send-message organ :command
                    `(:command :speak
                               :say (:agent :need
                                            :need :server
                                            :server (:name ,*forwarder-server* :port ,*forwarder-server-port* :hosts ("localhost")))))
      ;; Announce "need handler" for each route
      (dolist (route routes)
        (send-message organ :command
                      `(:command :speak
                                 :say (:agent :need
                                              :need :handler
                                              :handler (:server ,*forwarder-server* :hosts ,hosts :route ,(cdr route) :name ,(handler-name name (car route)))))))

      ;; Add forwarder to agent list
      (add-forwarder agent name need-info)

      ;; Then announce "need filled for forwarder"
      (send-message organ :command
                    `(:command :speak
                               :say (:filled :need
                                             :need ,what
                                             ,what ,need-info))))))

(defmethod agent-needs ((agent forwarder-agent) (organ agent-head) (what (eql :remove-forwarders)) need-info)
  "Removes the named forwarders."
  (labels ((from-info (thing) (getf need-info thing))
           (handler-name (name route) (format nil "forwarder-~A-~A" name route)))
    (let* ((names (from-info :names))
           (handlers-to-remove nil))
      (dolist (name names)
        (appendf handlers-to-remove (mapcar #'(lambda (x) (handler-name name (car x))) (routes-for-forwarder agent name))))

      (send-message organ :command
                    `(:command :speak
                               :say (:agent :need
                                            :need :remove-handlers
                                            :remove-handlers (:server ,*forwarder-server* :names ,handlers-to-remove))))
      (remove-forwarders agent names))

    ;; announce handler removal
    (send-message organ :command
                  `(:command :speak
                             :say (:filled :need
                                           :need ,what
                                           ,what ,need-info)))))

(defmethod agent-needs ((agent forwarder-agent) (organ agent-head) (what (eql :keep-forwarders)) need-info)
  "Removes all forwarders except those named."
  (labels ((from-info (thing) (getf need-info thing))
           (handler-name (name route) (format nil "forwarder-~A-~A" name route)))
    (let* ((names (from-info :names))
           (handlers-to-keep nil))
      (dolist (name names)
        (appendf handlers-to-keep (mapcar #'(lambda (x) (handler-name name (car x))) (routes-for-forwarder agent name))))

      (send-message organ :command
                    `(:command :speak
                               :say (:agent :need
                                            :need :keep-handlers
                                            :keep-handlers (:server ,*forwarder-server* :names ,handlers-to-keep))))
      (cull-forwarders agent names))

    ;; announce handler removal
    (send-message organ :command
                  `(:command :speak
                             :say (:filled :need
                                           :need ,what
                                           ,what ,need-info)))))

(defmethod heard-message ((agent forwarder-agent) (organ agent-head) (from (eql :agent)) (type (eql :need)) &rest request)
  (log-for (trace forwarder-agent) "Heard an :agent :need message: ~A" request)
  (let* ((need-what (getf request :need))
         (need-info (getf request need-what)))
    (agent-needs agent organ need-what need-info)))
