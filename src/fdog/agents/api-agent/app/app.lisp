(in-package :api-app)

(defcategory api-app)
(defgeneric api (handler request raw)
  (:documentation "The root of the API App.")
  (:method (handler request raw)
    (log-for (trace api-app) "Request: ~S" request)))
