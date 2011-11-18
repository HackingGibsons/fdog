(defpackage :fdog-models
  (:use :cl)
  (:use :sb-mop)
  (:use :external-program)
  (:shadowing-import-from :log5
                          :log-for :info :warn :error)
  (:shadowing-import-from :sb-posix
                          :syscall-error
                          :kill :sigint :sigterm :sighup
                          :chdir)
  (:export :connected-p :connect :disconnect :reconnect
           :*server-database*
           :*server-dir*
           :*config-file*

           ;; Helpers
           :model-pk

           ;; m2sh methods
           :init
           :make-server
           :make-host
           :make-route
           :make-dir
           :servers
           ;; Mongrel2 Configuration Classes
           :mongrel2-server
             ;; Data access
             :mongrel2-server-cert
             :mongrel2-server-id
             :mongrel2-server-ssl
             :mongrel2-server-ssl-p
             :mongrel2-server-name
             :mongrel2-server-uuid
             :mongrel2-server-chroot
             :mongrel2-server-root ;; Computed from defaults
             :mongrel2-server-pidfile
             :mongrel2-server-host ;; TODO: Make either a generic method, or a virtual slot
             :mongrel2-server-default-host
             :mongrel2-server-default-host-name
             :mongrel2-server-addr
             :mongrel2-server-port
             :mongrel2-server-hosts
             ;; More proper methods
             :mongrel2-server-pid
             :mongrel2-server-running-p
             :mongrel2-server-signal
             :mongrel2-server-signal/block
           :mongrel2-host
             :find-mongrel2-host
             :make-mongrel2-host
             :mongrel2-host-id
             :mongrel2-host-name
             :mongrel2-host-matching
             :mongrel2-host-routes
             :mongrel2-host-server
             :mongrel2-host-server-id
           :mongrel2-handler
             :mongrel2-handler-send-ident
             :mongrel2-handler-recv-ident
             :mongrel2-handler-send-spec
             :mongrel2-handler-recv-spec
             :find-mongrel2-handler
             :make-mongrel2-handler
           :mongrel2-proxy
           :mongrel2-directory
           :mongrel2-route
             :make-host-route
             :find-mongrel2-route
             :mongrel2-route-path
             :mongrel2-route-target
             :mongrel2-route-host
             :mongrel2-route-host-id
           :mongrel2-target-route
           :mongrel2-setting
             :make-mongrel2-setting
             :find-mongrel2-setting
             :mongrel2-setting-key
             :mongrel2-setting-value
           :mongrel2-mimetype
             :*default-mimetypes*
           :mongrel2-log
           :mongrel2-statistic))
(in-package :fdog-models)

;; Load the sqlite3 DB
(clsql:initialize-database-type :database-type :sqlite3)

;; TODO: This may be better expressed in CLOS, now that I've noticed what I'm doing..
;;   Re: Keeping state and a pile of methods to poke it

;; Knobs
(defvar *server-database* nil
  "A handle to the database connection to the server.")

(defvar *server-dir* '(:relative "server"))
(defvar *config-file* (make-pathname :name "config" :type "sqlite"))

(defvar *mongrel2-bin* "mongrel2"
  "Mongrel2 binary")

;; Methods
(defun connected-p ()
  "Bool of the current connection state"
  (and *server-database* (member *server-database* (clsql:connected-databases))))

(defun connect (&optional (db-path (merge-pathnames *config-file*
                                                    (merge-pathnames (make-pathname :directory *server-dir*)
                                                                     afdog:*root*))))
  "Connect to a mongrel2 configuration database."
  (log-for (dribble) "Connecting to database path: ~A" db-path)
  (unless (connected-p)
    (setf *server-database* (clsql:connect `(,(namestring db-path)) :database-type :sqlite3))))

(defun disconnect ()
  "Disconnect from any existing database connecting we have.
Return t if an action took place, nil otherwise"
  (when (connected-p)
    (clsql:disconnect :database *server-database*))
  (when *server-database*
    (not (setf *server-database* nil))))

(defun reconnect ()
  "Disconnect and reconnect to the database
Returns two values, the result of disconnecting and connecting"
  (let ((name (and (connected-p) (clsql:database-name *server-database*))))
    (values (disconnect)
            (connect name))))
