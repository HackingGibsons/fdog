(in-package :fdog-models)

;; Mongrel2 Server Generic Methods
(defgeneric mongrel2-server-running-p (server)
  (:documentation "Determine if a given server is running or not."))

(defmethod mongrel2-server-running-p ((server mongrel2-server))
  (let* ((pidfile (merge-pathnames (mongrel2-server-pidfile server)
                                   (mongrel2-server-root server)))
         (pid (and (probe-file pidfile)
                   (with-open-file (pid pidfile) (read pid)))))
    (and pid
         (eq (kill pid 0) 0))))

(defmethod mongrel2-server-pidfile :around ((server mongrel2-server))
  (let ((result (call-next-method)))
    (if (eql (char result 0) #\/)
        (subseq result 1)
      result)))

