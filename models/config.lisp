(in-package :fdog-models)

(clsql:def-view-class mongrel2-server ()
  ((id :type integer :db-kind :key)
   (uuid :type string)
   (access-log :type string)
   (error-log :type string)
   (chroot :type string
           :initform "/var/www")
   (pid-file :type string)
   (default-host :type string)
   (bind-addr :type string
              :initform "0.0.0.0"))
  (:base-table server))

(clsql:def-view-class mongrel2-host ()
  ((id :db-kind :key :type integer)
   (server-id :type integer :db-kind :key)
   (maintenance :type boolean)
   (name :type string)
   (matching :type string)

   (server :db-kind :join
           :db-info (:join-class mongrel2-server
                     :home-key server-id
                     :foreign-key id
                     :set nil)))
  (:base-table host))

(clsql:def-view-class mongrel2-handler ()
  ((id :db-kind :key :type integer)
    (send_spec :type string)
    (send_ident :type string)
    (recv_spec :type string)
    (recv_ident :type string)
    (raw_payload :type integer
                 :initform 0)
    (protocol :type string
              :initform "json"))
  (:base-table handler))
