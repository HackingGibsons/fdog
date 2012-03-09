(in-package :forwarder-agent)

(defcategory forwarder-kids)
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

    (log-for (warn forwarder-kids) "TODO: Forwarder-pairs: ~S" (all-route-pairs (forwarders agent)))))
