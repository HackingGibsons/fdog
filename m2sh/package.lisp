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
        (clsql:drop-view-from-class view-class)))))



