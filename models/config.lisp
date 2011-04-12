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
    (send-spec :type string)
    (send-ident :type string)
    (recv-spec :type string)
    (recv-ident :type string)
    (raw-payload :type integer
                 :initform 0)
    (protocol :type string
              :initform "json"))
  (:base-table handler))

(clsql:def-view-class mongrel2-proxy ()
  ((id :db-kind :key :type integer)
   (addr :type string)
   (port :type integer))
  (:base-table proxy))

(clsql:def-view-class mongrel2-directory ()
  ((id :db-kind :key :type integer)
   (base :type string)
   (index-file :type string)
   (default-ctype :type string))
  (:base-table directory))

(clsql:def-view-class mongrel2-route ()
  ((path :type string)
   (reversed :type boolean
             :init-form 0)
   (host-id :db-kind :key :type integer)
   (target-id :db-kind :key :type integer)    ;; TODO: This relation is not easily expressed in the ORM
   (target-type :db-kind :key :type string))  ;;       needs to be done with a :virtual slot and slot-value-using-class (?)
  (:base-table route))
