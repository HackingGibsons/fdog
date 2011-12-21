(in-package :mongrel2-agent)

(defgeneric agent-needs (agent organ need-what need-info)
  (:documentation "Called when an :agent :need message is heard by the agent for simplified dispatch.")
  (:method (agent organ need-what need-info)
    "Default method is a whiny no-op"
    (log-for (trace agent-needs) "~A/~A does not know how to fill the need for ~A using ~A"
             agent organ need-what need-info)))

(defmethod agent-needs ((agent mongrel2-agent) (organ agent-head) (what (eql :handler)) need-info)
  (labels ((from-info (thing) (getf need-info thing))

           (local-tcp-address (port)
             (format nil "tcp://~A:~A" (agent::get-local-address :as :string :update :please) port))

           (local-address-from-string (string &optional (base 20000))
             (local-tcp-address (string-to-integer string :base base :width 10000))))

    (let* ((server (awhen (from-info :server)
                     (fdog-models:servers :one t :refresh t :name it)))
           (handler (when server
                      (fdog-models:find-mongrel2-handler :ident (from-info :name) :exact nil)))
           (handler-ident (or (and handler (fdog-models:mongrel2-handler-send-ident handler))
                              (format nil "~A-~A" (from-info :name) (uuid:make-v4-uuid))))

           (handler (when server
                      (fdog-models:make-mongrel2-handler handler-ident
                                                         (local-address-from-string handler-ident 40000)
                                                         (local-address-from-string handler-ident 50000)
                                                         :recv-ident handler-ident
                                                         :update t)))
           (hosts (when handler
                    (mapcar (curry #'fdog-models:make-mongrel2-host server) (from-info :hosts)))))

      (and server hosts handler (from-info :route)
           (mapcar (rcurry #'fdog-models:make-host-route (from-info :route) handler)
                   hosts)
           (link-server organ server (clsql:database-name clsql:*default-database*)))

      (send-message organ :command `(:command :speak
                                     :say (:filled :need
                                                   :need ,what
                                                   ,what (:server ,(from-info :server)
                                                          :hosts ,(from-info :hosts)
                                                          :route ,(from-info :routes)
                                                          :name ,(from-info :name)
                                                          :endpoint (:push ,(fdog-models:mongrel2-handler-send-spec handler)
                                                                     :sub ,(fdog-models:mongrel2-handler-recv-spec handler)))))))))



(defmethod agent-needs ((agent mongrel2-agent) (organ agent-head) (what (eql :keep-hosts)) need-info)
  (flet ((from-info (thing) (getf need-info thing))
         (need-host (host)
           (find (fdog-models:mongrel2-host-name host) (getf need-info :hosts)
                 :test #'string=)))
    (let* ((server (fdog-models:servers :one t :name (getf need-info :server)))
           (hosts (remove-if #'need-host (and server (fdog-models:mongrel2-server-hosts server)))))

      (and (mapc #'fdog-models:remove-mongrel2-host hosts)
           (clsql:update-objects-joins (list server)))

      (if (and server (fdog-models:mongrel2-server-hosts server))
              (progn
                (unless (fdog-models:mongrel2-server-default-host server)
                  (setf (fdog-models:mongrel2-server-default-host-name server)
                        (car (mapcar #'fdog-models:mongrel2-host-name (fdog-models:mongrel2-server-hosts server))))
                  (clsql:update-records-from-instance server))

                (fdog-models:mongrel2-server-signal server :reload))
              (when server
                (unlink-server organ server (clsql:database-name clsql:*default-database*))
                (clsql:delete-instance-records server)))

      (and server hosts (send-message organ :command
           `(:command :speak
                      :say (:filled :need
                                    :need ,what
                                    ,what (:server ,(from-info :server)
                                           :hosts ,(mapcar #'fdog-models:mongrel2-host-name hosts)))))))))



(defmethod agent-needs ((agent mongrel2-agent) (organ agent-head) (what (eql :remove-host)) need-info)
  "A :need for a :server gets filled when heard."
  (flet ((from-info (thing) (getf need-info thing)))
    (let* ((server (fdog-models:servers :one t :name (from-info :server)))
           (host (find (from-info :host)
                       (and server (fdog-models:mongrel2-server-hosts server))
                       :key #'fdog-models:mongrel2-host-name
                       :test #'string-equal)))
      (log-for (agent-needs trace) "Found server: ~A host: ~A" server host)

      ;; TODO:
      ;; Extract enough methods to make the bellow clean to accomplish
      (when (and server host)
        (fdog-models:remove-mongrel2-host host)

        (let ((server (fdog-models:servers :one t  :refresh t
                                           :uuid (fdog-models:mongrel2-server-uuid server))))
          (if (and server (fdog-models:mongrel2-server-hosts server))
              (progn
                (unless (fdog-models:mongrel2-server-default-host server)
                  (setf (fdog-models:mongrel2-server-default-host-name server)
                        (car (mapcar #'fdog-models:mongrel2-host-name (fdog-models:mongrel2-server-hosts server))))
                  (clsql:update-records-from-instance server))

                (fdog-models:mongrel2-server-signal server :reload))
              (progn
                (unlink-server organ server (clsql:database-name clsql:*default-database*))
                (clsql:delete-instance-records server))))

        (send-message organ :command `(:command :speak
                                                :say (:filled :need
                                                              :need ,what
                                                              ,what (:server ,(from-info :server)
                                                                     :host ,(from-info :host)))))))))

(defmethod agent-needs ((agent mongrel2-agent) (organ agent-head) (what (eql :keep-servers)) need-info)
  (flet ((need-server-p (server)
           (find (fdog-models:mongrel2-server-name server) need-info
                 :test #'string=)))
    (let* ((servers (fdog-models:servers :refresh t))
           (remove (remove-if #'need-server-p servers)))

      (dolist (server remove remove)
        (unlink-server organ server (clsql:database-name clsql:*default-database*))
        (fdog-models:remove-mongrel2-server server))

      (send-message organ :command `(:command :speak
                                     :say (:filled :need
                                           :need ,what
                                           ,what ,(mapcar #'fdog-models:mongrel2-server-name remove)))))))




(defmethod agent-needs ((agent mongrel2-agent) (organ agent-head) (what (eql :remove-server)) need-info)
  "A :need for a :server gets filled when heard."
  (flet ((server-info-cons (server)
           "Format a server as a cons pair in the format of (name . uuid)"
           (and server
                (cons (fdog-models:mongrel2-server-name server)
                      (fdog-models:mongrel2-server-uuid server))))
         (get-servers (info) (apply #'fdog-models:servers :one t :refresh t info)))
    (let* ((servers (mapcar #'get-servers need-info)))
      (log-for (agent-needs trace) "Found servers to remove: ~A" servers)
      (dolist (server servers)
        (unlink-server organ server (clsql:database-name clsql:*default-database*))
        (send-message organ :command `(:command :speak
                                                :say (:filled :need
                                                              :need ,what
                                                              ,what ,(mapcar #'server-info-cons servers))))
        (fdog-models:remove-mongrel2-server server)))))


(defmethod agent-needs ((agent mongrel2-agent) (organ agent-head) (what (eql :server)) need-info)
  "A :need for a :server gets filled when heard."
  (flet ((from-info (thing) (getf need-info thing)))
    (let* ((server (or (fdog-models:servers :name (from-info :name) :refresh t :one t)
                       (fdog-models:make-server (from-info :name) :port (from-info :port))))
           (hosts (mapcar #'(lambda (host-name)
                              (log-for (trace agent-needs) "Building ~A for ~A" host-name server)
                              (fdog-models:make-mongrel2-host server host-name))
                          (and server (from-info :hosts)))))


      ;; TODO: Things like this should be in an :after for update-records-from-instance or something similar
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
