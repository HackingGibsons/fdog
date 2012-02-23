(in-package :forwarder-agent)


(defmethod agent-needs ((agent forwarder-agent) (organ agent-head) (what (eql :forwarder)) need-info)
  "Creates or updates a forwarder (\"forwarder server\" + named mongrel2 handler) in response to a need request."
  (labels ((from (place thing) (cdr (assoc thing place)))
           (from-info (thing) (from need-info thing)))
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
        (let ((route-name (from route :name))
              (path (from route :route)))
          (send-message organ :command
                        `(:command :speak
                                   :say (:agent :need
                                                :need :handler
                                                :handler (:server ,*forwarder-server* :hosts ,hosts :route ,path :name ,(handler-name name route-name)))))))

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
  (labels ((from (place thing) (cdr (assoc thing place)))
           (from-info (thing) (getf need-info thing)))
    (let* ((names (from-info :names))
           (handlers-to-remove nil))
      (dolist (name names)
        (appendf handlers-to-remove (mapcar #'(lambda (route) (handler-name name (from route :name))) (routes-for-forwarder agent name))))

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
  (labels ((from (place thing) (cdr (assoc thing place)))
           (from-info (thing) (getf need-info thing)))
    (let* ((names (from-info :names))
           (handlers-to-keep nil))
      (dolist (name names)
        (appendf handlers-to-keep (mapcar #'(lambda (route) (handler-name name (from route :name))) (routes-for-forwarder agent name))))

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

(defmethod heard-message :after ((agent forwarder-agent) (organ agent-head) (from (eql :agent)) (type (eql :info)) &rest info)
  "When hearing an announcement from an agent that provides mongrel2 servers,
if the `forwarder' server is missing handlers the agent expects, request the handlers."
  (log-for (trace forwarder-agent) "Heard an agent info message, info: ~A" info)
  (when-bind forwarder-server (assoc *forwarder-server* (getf (getf (getf info :info) :provides) :servers) :test #'string=)
    (log-for (trace forwarder-agent) "Heard an agent info message, forwarder server: ~A" forwarder-server)
    (labels ((handler-names-for-forwarder (forwarder)
               (let ((name (cdr (assoc :name (cdr forwarder))))
                     (routes (cdr (assoc :routes (cdr forwarder)))))
                 (mapcar #'(lambda (x) (handler-name name x)) (mapcar #'(lambda (x) (cdr (assoc :name x))) routes))))

             (handler-list (forwarders)
               (let ((handler-list nil))
                 (dolist (forwarder forwarders)
                   (appendf handler-list (handler-names-for-forwarder forwarder)))
                 handler-list)))

      (let* ((handlers (mapcar #'car (cdr forwarder-server)))
             (agent-forwarders (forwarders agent))
             (expected-handlers (handler-list agent-forwarders))
             (missing-handlers (set-difference expected-handlers handlers :test #'string=)))
        (when missing-handlers
          ;; Request all handlers, requesting a handler that already
          ;; exists is harmless
          (dolist (forwarder agent-forwarders)
            (labels ((from (place thing) (cdr (assoc thing place)))
                     (from-info (thing) (from (cdr forwarder) thing)))
              (let ((name (from-info :name))
                    (hosts (from-info :hosts))
                    (routes (from-info :routes)))
                ;; Announce "need handler" for each route
                (dolist (route routes)
                  (let ((route-name (from route :name))
                        (path (from route :route)))
                    (send-message organ :command
                                  `(:command :speak
                                             :say (:agent :need
                                                          :need :handler
                                                          :handler (:server ,*forwarder-server* :hosts ,hosts :route ,path :name ,(handler-name name route-name)))))))))))))))
