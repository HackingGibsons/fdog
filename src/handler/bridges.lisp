(in-package :fdog-handler)

(defvar *configured-bridges* nil
  "A list of all registred server -> handler-pair configurations.
Request handlers compiled from the mongrel2 configuration in the form:

 ((mongrel2-server . ((mongrel2-handler . request-handler)))
     .
     .
  (mongrel2-server-n . ((mongrel2-handler-n . request-handler-n))))")


(defmethod configure-bridges-for ((server mongrel2-server))
  "Configure bridges for all hosts and all routes of all of the server
given in `server'."
  :undef)

(defmethod configure-bridges-for ((host mongrel2-host))
  "Configures bridges for all routes of the host named in `host'"
  :undef)

(defmethod configure-bridges-for ((route mongrel2-route))
  :undef)

(defmethod configure-bridges-for ((handler mongrel2-handler))
  "Configures a bridge in accordance with the handler passed.
Returns just the matching request-handler instance."
  :undef)
