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
   (server :db-kind :join
           :db-info (:join-class mongrel2-server
                        :home-key server-id
                        :foreign-key id))
   (name :type string)
   (matching :type string))
  (:base-table host))
