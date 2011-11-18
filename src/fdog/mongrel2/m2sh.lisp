(in-package :fdog-models)

(defun init ()
  "Drops, then creates all the tables of the config"
  (let (view-classes)
    (do-symbols (view-class (find-package :fdog-models) view-classes)
      (when (typep (find-class view-class nil) 'clsql-sys::standard-db-class)
        (push view-class view-classes)
        (log-for (trace) "Dropping table named by viewclass ~A" view-class)
        (clsql:drop-view-from-class view-class)
        (log-for (trace) "Creating table named by viewclass ~A" view-class)
        (clsql:create-view-from-class view-class)))))

(defun make-server (name &rest args &key bind port
                    (chroot (merge-pathnames #P"server/" afdog:*root*))
                    &allow-other-keys)
  (let ((server (apply 'make-instance `(mongrel2-server :name ,name ,@args))))
    (clsql:update-records-from-instance server)
    server))

(defun servers (&key uuid host name (refresh nil) one)
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

    (let ((the-servers (apply 'clsql:select `(mongrel2-server ,@defaults ,@params))))
      (if one (car the-servers) the-servers))
  #.(clsql:restore-sql-reader-syntax-state)))
