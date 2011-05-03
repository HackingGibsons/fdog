(defpackage :fdog-m2sh
  (:use :cl
        :fdog-models)
  (:use :sb-mop)
  (:shadowing-import-from :log5
                          :log-for :info :warn :error)
  (:export :servers
           :server-hosts
           :init))
(in-package :fdog-m2sh)

(defun server-hosts (server &key (refresh nil))
  "Returns a list of hosts configured for a given server with name server-name"
  #.(clsql:locally-enable-sql-reader-syntax)
  (let (hosts)
    (dolist (host (clsql:select 'mongrel2-host :flatp t :refresh refresh
                                :where [= 'server_id (mongrel2-server-id server)])
             hosts)
      (push host hosts)))
  #.(clsql:restore-sql-reader-syntax-state))

(defun servers (&key uuid host name (refresh nil))
  "Return a list of servers given a :uuid and :host
Omitted, all servers are returned"
  #.(clsql:locally-enable-sql-reader-syntax)
  (let (params
        (defaults `(:flatp t :refresh ,refresh)))
    (when name
      (push [= 'name name] params))
    (when uuid
      (push [= 'uuid uuid] params))
    (when host
      (push [= 'default_host host] params))
    (when params
      (setf params `(:where ,(eval (car `([and ,@params]))))))

    (apply 'clsql:select `(mongrel2-server ,@defaults ,@params)))
  #.(clsql:restore-sql-reader-syntax-state))

(defun init ()
  "Drops, thenc creates all the tables of the config"
  (let (view-classes)
    (do-symbols (view-class (find-package :fdog-models) view-classes)
      (when (typep (find-class view-class nil) 'clsql-sys::standard-db-class)
        (push view-class view-classes)
        (log-for (trace) "Dropping table named by viewclass ~A" view-class)
        (clsql:drop-view-from-class view-class)
        (log-for (trace) "Creating table named by viewclass ~A" view-class)
        (clsql:create-view-from-class view-class)))))



;;; TODO: WIP: Construction of configuration construction
`(defun sketch! ()
  (server% (:name "default" :addr "localhost" :port 1337 :chroot "./")
    (host% ("superlocalhost")
            (route% "/dir/" (dir% "data/")))
    (host% ("localhost")
           (route% "/proxy/" (proxy% "localhost" 31337))
           (route% "/handler/" (handler% :send-spec "tcp://127.0.0.1:9999"
                                         :send-ident "54c6755b-9658-40f4-9c2a-fe81a816345e"
                                         :recv-spec "tcp://127.0.0.1:9998")))))

(defun using-configuration! (&rest servers)
  "Clear the configuration and install the configuration of each server that
appears in the `servers' list."
  :undefined)

(defmacro with-server ((name &rest args &key addr port chroot) &body hosts)
  `(let ((server (make-server ,name ,@args)))
     (flet ((attach-server-to-host (host)
              (setf (slot-value host 'fdog-models::server-id)
                    (slot-value server 'fdog-models::id))
              (clsql:update-records-from-instance host)))
       (mapc #'attach-server-to-host (list ,@hosts))
       server)))

(defmacro with-host ((name) &body routes)
  `(let ((host (make-host ,name)))
     (flet ((attach-host-to-route (route)
              (setf (slot-value route 'fdog-models::host-id)
                    (slot-value host 'fdog-models::id))
              (clsql:update-records-from-instance route)))
       (mapc #'attach-host-to-route (list ,@routes))
       host)))

;; vv- Macroexpansion of a nested server definiton should compile to something resembling this
'(let ((server (make-server "default" :addr "localhost" :port 1337 :chroot "./")))
  (defun attach-server-to-host (host)
    (setf (slot-value host 'fdog-models::server-id)
          (slot-value server 'fdog-models::id))
    (clsql:update-records-from-instance host))

  (mapc #'attach-server-to-host
        `(,(let ((host (make-host "superlocalhost")))
                (defun attach-host-to-route (route)
                  (describe host)
                  (setf (slot-value route 'fdog-models::host-id)
                        (slot-value host 'fdog-models::id))
                  (clsql:update-records-from-instance route))

                (mapc #'attach-host-to-route
                      `(,(make-route "/dir/" (make-dir "data/"))))
                host)
           ,(let ((host (make-host "localhost")))
                 (defun attach-host-to-route (route)
                   (setf (slot-value route 'fdog-models::host-id)
                         (slot-value host 'fdog-models::id))
                   (clsql:update-records-from-instance route))

                 (mapc #'attach-host-to-route
                       `(,(make-route "/proxy/" (make-proxy "localhost" 31337))
                          ,(make-route "/handler/" (make-handler :send-spec "tcp://127.0.0.1:9999"
                                                                 :send-ident "54c6755b-9658-40f4-9c2a-fe81a816345e"
                                                                 :recv-spec "tcp://127.0.0.1:9998"))))
                 host)))
  server)

;; Actual construction of components
(defun make-server (name &rest args &key addr port
                    (chroot (merge-pathnames fdog:*default-server-path* fdog:*default-root-path*))
                    &allow-other-keys)
  (let ((server (apply 'make-instance `(mongrel2-server :name ,name ,@args))))
    (clsql:update-records-from-instance server)
    server))

(defun make-host (name &optional matching)
  (let ((host (make-instance 'mongrel2-host :name name)))
    (when matching
      (setf (slot-value host 'fdog-models::matching) matching))
    (clsql:update-records-from-instance host)
    host))

(defun make-route (path target)
  (let ((route (make-instance 'mongrel2-route :path path)))
    (setf (slot-value route 'fdog-models::target) target)
    (clsql:update-records-from-instance route)
    route))

(defun make-handler (&rest initargs)
  (let ((handler (apply 'make-instance `(mongrel2-handler ,@initargs))))
    (clsql:update-records-from-instance handler)
    handler))

(defun make-proxy (addr port)
  (let ((proxy (make-instance 'mongrel2-proxy :addr addr :port port)))
    (clsql:update-records-from-instance proxy)
    proxy))

(defun make-dir (base &optional (index "index.html"))
  (let ((dir (make-instance 'mongrel2-directory :base base :index index)))
    (clsql:update-records-from-instance dir)
    dir))
