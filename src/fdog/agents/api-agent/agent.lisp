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
   (endpoints
    :accessor endpoints
    :initform nil
    :documentation "A list of endpoints the agent knows about.")
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

    ;; If the announce is from a req-processing agent,
    ;; see what forwarder it handles and update the information
    ;; for that matching (forwarder . route) pair

    ;; TODO deleting will be handled by looking for :filled
    ;; :remove-forwarder messages
    (let ((forwarding (getf provides :forwarding :not-found)))
      (unless (eql forwarding :not-found)
        (let ((forwarder (cdr (assoc :forwarder forwarding)))
              (route (cdr (assoc :route forwarding)))
              (path (cdr (assoc :path forwarding)))
              (endpoints (endpoints agent)))
          ;; Check if the endpoint exists. If it does, remove the old
          ;; endpoint announce.
          ;; (Last announce wins since each endpoint is announced by a
          ;; separate agent)
          (when-bind endpoint (find-if #'(lambda (e) (and
                                                 (string= forwarder (cdr (assoc :forwarder e)))
                                                 (string= route (cdr (assoc :route e)))
                                                 (string= path (cdr (assoc :path e)))))
                                       endpoints)
            (setf endpoints (remove endpoint endpoints)))

          ;; Then append the endpoint we saw to the agent's list
          (appendf (endpoints agent) (list forwarding)))))))
