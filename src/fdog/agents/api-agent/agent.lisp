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
  ()
  (:default-initargs . (:handle *api-handler*))
  (:documentation "This agent establishes a handler for the API endpoint
and contains the implementation of the afdog API."))

(defmethod request-handler ((agent api-agent) organ req raw)
  "Request handler."
  (api-app:api agent organ (handler agent) req raw))

(defmethod heard-message :after ((agent api-agent) (head agent-head) (from (eql :agent)) (info (eql :info)) &rest event)
  (let* ((info (getf event :info))
         (provides (getf info :provides))
         (servers (getf provides :servers))
         (control (assoc *control-server* servers :test #'string=)))
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
          (log-for (trace api-agent) "Server: ~S doesn't have handler ~S, adding." name (handler-name agent)))))))
