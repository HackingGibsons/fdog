(in-package :fdog-handler)

(defmethod configure-bridges-for ((handler mongrel2-handler))
  "Configures a bridge in accordance with the handler passed.
Returns just the matching request-handler instance."
  (make-instance 'request-handler :ident (mongrel2-handler-send-ident handler)
                 :db-handler handler
                 :sub (mongrel2-handler-send-spec handler)
                 :pub (mongrel2-handler-recv-spec handler)))
