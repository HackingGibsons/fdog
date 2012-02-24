(in-package :api-agent)

;; Knobs
(defvar *control-server* "control"
  "The name of the control server that we should ask for an API handler on")
(defvar *api-handler* "api"
  "The name of the handler that hosts the API")
(defvar *api-host* "localhost"
  "The host to use for the API agent.")

;; Agent
(defcategory api-agent)
(defclass api-agent (request-processing-agent standard-leaf-agent)
  ((forwarders
    :accessor forwarders
    :initform nil
    :documentation "A list of forwarders the agent knows about.")
   (handlers
    :accessor handlers
    :initform nil
    :documentation "A list of handlers the agent knows about.")
   (callbacks
    :accessor callbacks
    :initform nil
    :documentation "List of callbacks registered to the agent."))
  (:default-initargs . (:handle *api-handler*))
  (:documentation "This agent establishes a handler for the API endpoint
and contains the implementation of the afdog API."))

(defmethod heard-message :after ((agent api-agent) (head agent-head) (from (eql :agent)) (info (eql :info)) &rest event)
  (let* ((info (getf event :info))
         (provides (getf info :provides))
         (servers (getf provides :servers))
         (control (assoc *control-server* servers :test #'string=)))
    ;; If API handler does not exist, announce need
    (multiple-value-bind (name handlers) (values (car control) (cdr control))
      (let ((handler (cdr (assoc (handler-name agent) handlers :test #'string=))))
        (when (and name (not handler))
          (send-message head :command `(:command :speak
                                        :say (:agent :need
                                              :need  :handler
                                              :handler (:server ,*control-server*
                                                        :hosts (,*api-host*)
                                                        :route "/"
                                                        :name ,(handler-name agent)))))
          (log-for (trace api-agent) "Server: ~S doesn't have handler ~S, adding." name (handler-name agent)))))

    ;; If the announce is from a forwarder agent, update the list of
    ;; forwarders
    (let ((forwarders (getf provides :forwarders :not-found)))
      ;; Differentiate between agent not providing forwarders, and agent
      ;; with (:PROVIDES (:FORWARDERS NIL)
      (unless (eql forwarders :not-found)
        (setf (forwarders agent) forwarders)))

    ;; If the announce is from a mongrel2 agent, look for a
    ;; "forwarder" server. If found, update the stored handler sockets
    ;; on the agent.
    (let ((servers (getf provides :servers :not-found)))
      (unless (eql servers :not-found)
        (when-bind forwarder-server (assoc "forwarder" servers :test #'string=)
          (let ((handlers (cdr forwarder-server)))
            (setf (handlers agent) handlers)))))))
