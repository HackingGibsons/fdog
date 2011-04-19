(defpackage :fdog-m2sh
  (:use :cl
        :fdog-models)
  (:use :sb-mop)
  (:export :servers))
(in-package :fdog-m2sh)

(defun servers (&key uuid host)
  "Return a list of servers given a :uuid and :host
Omitted, all servers are returned"
  #.(clsql:locally-enable-sql-reader-syntax)
  (let (params)
    (when uuid
      (push [= 'uuid uuid] params))
    (when host
      (push [= 'default_host host] params))
    (when params
      (setf params `(:where ,(eval (car `([and ,@params]))))))

    (apply 'clsql:select `(mongrel2-server :flatp t ,@params)))
  #.(clsql:restore-sql-reader-syntax-state))
