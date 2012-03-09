(in-package :forwarder-agent)

(defcategory forwarder-kids)
(defmethod find-forwarding-agent-cell ((agent forwarder-agent) name &optional (make t))
  (or (assoc name (forwarder-agents agent) :test #'string-equal)
      (and make
           (let ((name-cell (cons name (list))))
             (appendf (forwarder-agents agent) (list name-cell))
             name-cell))))

(defmethod spawn-forwarder ((agent forwarder-agent) name route)
  (let ((uuid (princ-to-string (uuid:make-v4-uuid))))
    (prog1 uuid
      (log-for (warn forwarder-kids) "TODO: Make agent: ~S=>~S: ~A" name route uuid))))

(defmethod maybe-spawn-forwarder ((agent forwarder-agent) name route)
  (let ((name-cell (find-forwarding-agent-cell agent name)))
    (unless (assoc route (cdr name-cell) :test #'string-equal)
      (push (cons route (spawn-forwarder agent name route))
            (cdr name-cell)))))

(defmethod maybe-heard-blacklist-agent ((agent forwarder-agent) (organ agent-head) info)
  (let ((uuid (getf (getf info :info) :uuid)))
    (when (find uuid (dead-agents agent) :test #'string-equal)
      (log-for (warn forwarder-kids) "Killing from blacklist: ~S" uuid)
      (send-message organ :command `(:command :speak
                                              :say (:agent :kill :kill ,uuid))))))

(defmethod kill-forwarder ((agent forwarder-agent) name route)
  (when-bind agent-cell (find-forwarding-agent-cell agent name nil)
    (when-bind uuid (cdr (assoc route (cdr agent-cell) :test #'string-equal))
      (log-for (warn forwarder-kids) "TODO: Perform an unwatch on: ~A" uuid)
      (pushnew uuid (dead-agents agent) :test #'string-equal))))

(defmethod maybe-kill-forwarder ((agent forwarder-agent) name route)
  (unless (find route (routes-for-forwarder agent name)
                :key #'(lambda (e) (cdr (assoc :name e)))
                :test #'string-equal)
    (kill-forwarder agent name route)))

(defmethod save-forwarders :after ((agent forwarder-agent))
  (labels ((forwarder-name (fwd)
             (car fwd))

           (forwarder-routes (fwd)
             (cdr (assoc :routes (cdr fwd))))

           (route-name (route)
             (cdr (assoc :name route)))

           (route-pairs (fwd)
             (loop for route in (forwarder-routes fwd)
                  collecting (list (forwarder-name fwd) (route-name route))))

           (all-route-pairs (fwds)
             (reduce #'append (mapcar #'route-pairs fwds))))

    (mapc (curry #'apply #'maybe-spawn-forwarder agent)
          (all-route-pairs (forwarders agent)))

    (dolist (name-cell (forwarder-agents agent))
      (dolist (route (cdr name-cell))
        (maybe-kill-forwarder agent (car name-cell) (car route))))))
