(in-package :fdog-forwarder)

(defmethod forward-request-handler :around (handler request raw &key interface)
  (log-for (trace) "Request processor for param rewrite.")
  (let ((msg raw))
    (call-next-method handler request msg :interface interface)))

(defmethod forward-request-handler (handler request raw &key interface)
  (declare (ignorable handler raw))
  (log-for (trace) "Request: ~A" request)
  (log-for (trace) "Interface: ~A" interface)
  ;; TODO: Rewrite the request to mention a UUID unique to this instance
  (with-slots (request-sock) interface
    (zmq:send request-sock (make-instance 'zmq:msg :data raw))))


