(in-package :api-agent)

;; Agent
(defclass api-agent (request-processing-agent)
  ()
  (:documentation "This agent establishes a handler for the API endpoint
and contains the implementation of the afdog API."))
