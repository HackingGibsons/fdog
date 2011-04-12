(in-package :fdog-models)

(clsql:def-view-class mongrel2-server ()
  ((id :type integer :db-kind :key
       :db-constraints '(:unique :auto-increment))
   (uuid :type string)
   (access-log :type string)
   (error-log :type string)
   (chroot :type string
           :initform "/var/www")
   (pid-file :type string)
   (default-host :type string)
   (bind-addr :type string
              :initform "0.0.0.0"))
  (:base-table server
   :documentation
   "Mongrel2 Server configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-260003.4.1"))

(clsql:def-view-class mongrel2-host ()
  ((id :db-kind :key :type integer
       :db-constraints '(:unique :auto-increment))
   (server-id :type integer :db-kind :key)
   (maintenance :type boolean)
   (name :type string)
   (matching :type string)

   (server :db-kind :join
           :db-info (:join-class mongrel2-server
                     :home-key server-id
                     :foreign-key id
                     :set nil)))
  (:base-table host
   :documentation
   "Mongrel2 Host configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-270003.4.2"))

(clsql:def-view-class mongrel2-route ()
  ((path :type string)
   (reversed :type boolean
             :init-form 0)
   (host-id :db-kind :key :type integer)
   (target-id :db-kind :key :type integer)    ;; TODO: This relation is not easily expressed in the ORM
   (target-type :db-kind :key :type string))  ;;       needs to be done with a :virtual slot and slot-value-using-class (?)
  (:base-table route
   :documentation
   "Mongrel2 Route configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-280003.4.3"))

(clsql:def-view-class mongrel2-handler ()
  ((id :db-kind :key :type integer
       :db-constraints '(:unique :auto-increment))
    (send-spec :type string)
    (send-ident :type string)
    (recv-spec :type string)
    (recv-ident :type string)
    (raw-payload :type integer
                 :initform 0)
    (protocol :type string
              :initform "json"))
  (:base-table handler
   :documentation
   "Mongrel2 Handler endpoint configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-310003.4.6"))

(clsql:def-view-class mongrel2-directory ()
  ((id :db-kind :key :type integer
       :db-constraints '(:unique :auto-increment))
   (base :type string)
   (index-file :type string)
   (default-ctype :type string))
  (:base-table directory
   :documentation
   "Mongrel2 Directory endpoint configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-290003.4.4"))

(clsql:def-view-class mongrel2-proxy ()
  ((id :db-kind :key :type integer
       :db-constraints '(:unique :auto-increment))
   (addr :type string)
   (port :type integer))
  (:base-table proxy
   :documentation
   "Mongrel2 Proxy endpoint configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-300003.4.5"))

(clsql:def-view-class mongrel2-setting ()
  ((id :db-kind :key :type integer
       :db-constraints '(:unique :auto-increment))
   (key :type string)
   (value :type string))
  (:base-table setting
   :documentation
   "Mongrel2 internal settings: http://mongrel2.org/static/mongrel2-manual.html#x1-380003.10"))

(clsql:def-view-class mongrel2-log ()
  ((id :db-kind :key :type integer
       :db-constraints '(:unique :auto-increment))
   (who :type string)
   (what :type string)
   (location :type string)
   (happened-at :type clsql:wall-time)
   (how :type string)
   (why :type string))
  (:base-table log
   :documentation "Mongrel2 config modification log"))

(clsql:def-view-class mongrel2-mimetype ()
  ((id :db-kind :key :type integer
       :db-constraints '(:primary-key :auto-increment))
   (mimetype :type string)
   (extension :type string))
  (:base-twable mimetype
   :documentation "Mongrel2 table of known mimetypes"))

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
  (:base-table statistic
   :documentation "Mongrel2 perfomance statistics table. Unused as of writing."))
