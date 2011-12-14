(in-package :mongrel2-agent)

(defgeneric agent-needs (agent organ need-what need-info)
  (:documentation "Called when an :agent :need message is heard by the agent for simplified dispatch.")
  (:method (agent organ need-what need-info)
    "Default method is a whiny no-op"
    (log-for (trace agent-needs) "~A/~A does not know how to fill the need for ~A using ~A"
             agent organ need-what need-info)))

(defmethod agent-needs ((agent mongrel2-agent) (organ agent-head) (what (eql :remove-server)) need-info)
  "A :need for a :server gets filled when heard."
  (flet ((server-info-cons (server)
           "Format a server as a cons pair in the format of (name . uuid)"
           (and server
                (cons (fdog-models:mongrel2-server-name server)
                      (fdog-models:mongrel2-server-uuid server))))
         (from-info (thing) (getf need-info thing))
         (get-servers (info) (apply #'fdog-models:servers :one t :refresh t info)))
    (let* ((servers (mapcar #'get-servers need-info)))
      (log-for (agent-needs trace) "Found servers to remove: ~A" servers)
      (dolist (server servers)
        (let* ((hosts (fdog-models:mongrel2-server-hosts server))
               (routes (loop for host in hosts
                          appending (fdog-models:mongrel2-host-routes host)))
               (targets (loop for route in routes
                           appending (fdog-models:mongrel2-route-target route))))
          (log-for (agent-needs trace) "Found hosts: ~A" hosts)
          (log-for (agent-needs trace) "Found routes: ~A" routes)
          (log-for (agent-needs trace) "Found targets: ~A" targets)

          (unlink-server organ server (clsql:database-name clsql:*default-database*))

          (mapc #'clsql:delete-instance-records
                (append servers hosts routes targets))

          (send-message organ :command `(:command :speak
                                          :say (:filled :need
                                                :need ,what
                                                ,what ,(mapcar #'server-info-cons servers)))))))))

(defmethod agent-needs ((agent mongrel2-agent) (organ agent-head) (what (eql :server)) need-info)
  "A :need for a :server gets filled when heard."
  (flet ((from-info (thing) (getf need-info thing)))
    (let* ((server (or (fdog-models:servers :name (from-info :name) :refresh t :one t)
                       (fdog-models:make-server (from-info :name) :port (from-info :port))))
           (hosts (mapcar #'(lambda (host-name)
                              (log-for (trace agent-needs) "Building ~A for ~A" host-name server)
                              (fdog-models:make-mongrel2-host server host-name))
                          (and server (from-info :hosts)))))


      (unless (fdog-models:mongrel2-server-default-host server)
        (setf (fdog-models:mongrel2-server-default-host-name server)
              (car (mapcar #'fdog-models:mongrel2-host-name hosts)))
        (log-for (trace agent-needs) "Updating default host on server: ~A" server))

      (setf (fdog-models:mongrel2-server-chroot server)
            (namestring (ensure-mongrel2-root-layout (agent-root agent))))

      (clsql:update-records-from-instance server)

      (and server hosts
           (link-server organ server (clsql:database-name clsql:*default-database*))
           (send-message organ :command `(:command :speak
                                          :say (:filled :need
                                                :need :server
                                                :server ,need-info)))))))


(defmethod heard-message ((agent mongrel2-agent) (organ agent-head) (from (eql :agent)) (type (eql :need)) &rest request)
  (log-for (trace mongrel2-agent) "Heard an :agent :need message: ~A" request)
  (let* ((need-what (getf request :need))
         (need-info (getf request need-what)))
    (agent-needs agent organ need-what need-info)))
