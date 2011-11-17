(defpackage #:mongrel2-agent
  (:documentation "Mongrel2 afdog agent and compontents.")
  (:use #:cl)
  (:use #:agent
        #:log5))

;(clsql:initialize-database-type :database-type :sqlite3)
