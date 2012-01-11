(in-package :request-processing-agent)

(defmethod heard-message ((agent standard-agent) (organ agent-requesticle) (from (eql :agent)) (type (eql :info)) &rest info)
  "The requesticle hears an info message. It should examine it to see if contains
any handlers we're interested in and we should connect to them."
  (log-for (trace requesticle) "Requesticle hears info: ~S" info))
