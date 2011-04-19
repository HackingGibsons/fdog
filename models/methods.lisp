(in-package :fdog-models)

;; Mongrel2 Server Generic Methods
(defgeneric mongrel2-server-running-p (server)
  (:documentation "Determine if a given server is running or not."))

(defmethod mongrel2-server-running-p ((server mongrel2-server))
  (format t "Is the server ~A running?~%" server)
  nil)
