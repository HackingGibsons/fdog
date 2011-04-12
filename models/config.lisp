(in-package :fdog-models)

(clsql:def-view-class mongrel2-server ()
  ((id :db-kind :key :type integer)
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
