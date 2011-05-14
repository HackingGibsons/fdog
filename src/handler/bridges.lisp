(in-package :fdog-handler)

(defclass handler-bridge (request-handler)
  ((db-handler :initarg :db-handler
               :reader handler-bridge-db-handler))
  (:documentation "A request-handler consructed from, and maintaining a Mongrel2 handler configurations."))

(defmethod configure-bridges-for ((handler mongrel2-handler))
  "Configures a bridge in accordance with the handler passed.
Returns just the matching request-handler instance."
  (make-instance 'handler-bridge :ident (mongrel2-handler-send-ident handler)
                 :db-handler handler
                 :sub (mongrel2-handler-send-spec handler)
                 :pub (mongrel2-handler-recv-spec handler)))
