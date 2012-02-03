(in-package :api-agent)

;; Agent
(defcategory api-agent)
(defclass api-agent (request-processing-agent)
  ()
  (:default-initargs . (:handle "api"))
  (:documentation "This agent establishes a handler for the API endpoint
and contains the implementation of the afdog API."))

(defmethod heard-message :after ((agent api-agent) (head agent-head) (from (eql :agent)) (info (eql :info)) &rest event)
  (let* ((info (getf event :info))
         (provides (getf info :provides))
         (servers (getf provides :servers))
         (control (assoc "control" servers :test #'string=)))
    (multiple-value-bind (name handlers) (values (car control) (cdr control))
      (let ((handler (cdr (assoc (handler-name agent) handlers :test #'string=))))
        (when (and name (not handler))
          (log-for (trace api-agent) "Server: ~S doesn't have handler ~S" name (handler-name agent)))))))
