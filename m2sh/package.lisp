(defpackage :fdog-m2sh
  (:use :cl
        :fdog-models)
  (:use :sb-mop)
  (:shadowing-import-from :log5
                          :log-for :info :warn :error)
  (:export :servers
           :init))
(in-package :fdog-m2sh)

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

;; vv- Macroexpansion of a nested server definiton should compile to something resembling this
'(let ((server '(make-server :name "default" :addr "localhost" :port 1337 :chroot "./")))
  (defun attach-server-to-host (host)
    (setf (slot-value host 'server) server))

  (mapc #'attach-server-to-host
        `(,(let ((host (make-host "superlocalhost")))
                (defun attach-host-to-route (route)
                  (setf (slot-value route 'host-id)
                        (slot-value host 'id)))

                (mapc #'attach-host-to-route
                      `(,(make-route "/dir/" (make-dir "data/"))))
                host)
          ,(let ((host (make-host "localhost")))
                (defun attach-host-to-route (route)
                  (setf (slot-value route 'host-id)
                        (slot-value host 'id)))

                (mapc #'attach-host-to-route
                      `(,(make-route "/proxy/" (make-proxy "localhost" 31337))
                        ,(make-route "/handler/" (make-handler :send-spec "tcp://127.0.0.1:9999"
                                                               :send-ident "54c6755b-9658-40f4-9c2a-fe81a816345e"
                                                               :recv-spec "tcp://127.0.0.1:9998"))))
                host)))
  server)

;; Actual construction of components
(defun make-route (path target)
  (let ((route (make-instance 'mongrel2-route :path path)))
    (setf (slot-value route 'fdog-models::target) target)
    (clsql:update-records-from-instance route)
    (clsql:update-instance-from-records route)
    route))

(defun make-handler (&rest initargs)
  (let ((handler (apply 'make-instance `(mongrel2-handler ,@initargs))))
    (clsql:update-records-from-instance handler)
    (clsql:update-instance-from-records handler)
    handler))

(defun make-proxy (addr port)
  (let ((proxy (make-instance 'mongrel2-proxy :addr addr :port port)))
    (clsql:update-records-from-instance proxy)
    (clsql:update-instance-from-records proxy)
    proxy))

(defun make-dir (base &optional (index "index.html"))
  (let ((dir (make-instance 'mongrel2-directory :base base :index index)))
    (clsql:update-records-from-instance dir)
    (clsql:update-instance-from-records dir)
    dir))
