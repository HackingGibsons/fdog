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

(clsql:def-view-class mongrel2-setting ()
  ((id :db-kind :key :type integer)
   (key :type string)
   (value :type string))
  (:base-table setting))

(clsql:def-view-class mongrel2-log ()
  ((id :db-kind :key :type integer
       :db-constraints '(:unique :auto-increment))
   (who :type string)
   (what :type string)
   (location :type string)
   (happened-at :type clsql:wall-time)
   (how :type string)
   (why :type string))
  (:base-table log))

(clsql:def-view-class mongrel2-mimetype ()
  ((id :db-kind :key :type integer
       :db-constraints '(:primary-key :auto-increment))
   (mimetype :type string)
   (extension :type string))
  (:base-twable mimetype))

;; This table exists largely for the future..
;; (maybe)
(clsql:def-view-class mongrel2-statistic ()
  ((id :type integer :db-kind :key
       :db-constraints '(:unique :auto-increment))

   ;; These /should/ be a c-pk
   (other-type :db-kind :key :type string)
   (other-id :db-kind :key :type integer)
   (name :db-kind :key :type string)
   ;; End of c-pk

   (sum :type float)
   (sumsq :type float)
   (n :type integer)
   (min :type float)
   (max :type float)
   (mean :type float)
   (sd :type float))
  (:base-table statistic))
