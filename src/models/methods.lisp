(in-package :fdog-models)

;;; Mongrel2 Server Generic Methods
(defgeneric mongrel2-server-running-p (server)
  (:documentation "Determine if a given server is running or not."))

(defgeneric mongrel2-server-signal (server signal)
  (:documentation ":start :stop :restart :reload :status a given server"))

(defgeneric mongrel2-server-pid (server)
  (:documentation "Get the numeric pid of the server, or nil if it's not running"))

(defgeneric mongrel2-server-config (server)
  (:documentation "Get the path of the config for a given mongrel2 server"))

;; Helper
(defmethod model-pk (obj)
  (slot-value obj 'id))

;;; Method specializations
(defmethod mongrel2-server-signal/block ((server mongrel2-server) signal
                                         &key (until-p 'mongrel2-server-running-p)
                                         invert (timeout 10) (pause 0.01))
  "Poll for a maximum `timeout' seconds waiting for `until-p' to become true
 (or false if `invert' is nonnil) at a poll interval of `pause' after sending the
`server' `signal'

If (and (eql `signal' :stop) (eql until-p 'mongrel2-server-running-p)) invert is automatically true.

`until-p' is called with `server' as the only parameter"
  (and (eql signal :stop)
       (eql until-p 'mongrel2-server-running-p)
       (setf invert t))

  (do ((sig-res (mongrel2-server-signal server signal) sig-res)
       (elapsed 0 (incf elapsed pause))
       (ready? (funcall until-p server) (funcall until-p server)))

      ((or (> elapsed timeout)
           (if invert (not ready?) ready?))

       (or (and (> elapsed timeout) :timeout)
           sig-res))

    (sleep pause)))

(defmethod mongrel2-server-signal ((server mongrel2-server) signal)
  (let ((running (mongrel2-server-running-p server)))
    (ecase signal
      (:start (unless running
                (let ((root (mongrel2-server-root server))
                      (config (make-pathname :directory nil
                                             :defaults (mongrel2-server-config server)))
                      (uuid (mongrel2-server-uuid server)))
                  (chdir root)
                  (log-for (trace) "Running (in: ~A): ~A ~A" root fdog:*mongrel2-bin* `(,config ,uuid))
                  (start fdog:*mongrel2-bin* `(,config ,uuid) :output t :error t))
                :started))

      (:stop (when running
               (log-for (trace) "Stopping ~A" server)
               (handler-case (progn
                               (kill (mongrel2-server-pid server) sigint) ;; TODO: Accept &optionals to upgrade to sigterm
                               :stopped)
                 (syscall-error () nil))))

      (:restart (mongrel2-server-signal server :stop)
                ;; Wait for the old process to die
                ;; before starting another
                (loop while (mongrel2-server-running-p server)
                   do (sleep 0.01))
                (mongrel2-server-signal server :start))

      (:reload (when running
                 (handler-case (progn
                                 (kill (mongrel2-server-pid server) sighup)
                                 :reloaded)
                   (syscall-error () nil))))

      (:status (if running :running :stopped)))))


(defmethod mongrel2-server-running-p ((server mongrel2-server))
  "T or NIL on (Running or Not-Running given a mongrel2-server
Returns true of it can find a pidfile, and a process is running there."
  (let* ((pid (mongrel2-server-pid server))
         (running (when pid
                    (handler-case (kill pid 0)
                      (syscall-error () nil)))))
    (and running (= running 0))))

(defmethod mongrel2-server-pid ((server mongrel2-server))
  (let ((pidfile (merge-pathnames (mongrel2-server-pidfile server)
                                  (mongrel2-server-root server))))
    (when (probe-file pidfile)
      (handler-case (with-open-file (pid pidfile) (read pid))
        (end-of-file () nil)))))

(defmethod mongrel2-server-config ((server mongrel2-server))
  (merge-pathnames fdog:*default-server-database-path*
                   (mongrel2-server-root server)))

(defmethod mongrel2-server-default-host ((server mongrel2-server))
  "The default host object for a given server"
  #.(clsql:locally-enable-sql-reader-syntax)
  (car (clsql:select 'mongrel2-host :flatp t :refresh t
                     :where [and [= 'server_id (model-pk server)]
                                 [= 'name (mongrel2-server-default-host-name server)]]))

  #.(clsql:restore-sql-reader-syntax-state))

;;; Model API Wrappers
(defgeneric mongrel2-server-cert (server type)
  (:documentation "Try to find a mongrel2 `server' SSL certificate of type `type'")
  (:method (server (type (eql :both)))
    "Return both the :crt and the :key for `server'"
    (list
     (mongrel2-server-cert server :crt)
     (mongrel2-server-cert server :key)))

  (:method ((server mongrel2-server) (type (eql :crt)))
    "Return the path to the .crt file for `server'"
    (mongrel2-server-cert server "crt"))

  (:method ((server mongrel2-server) (type (eql :key)))
    "Return the path to the .key file for `server'"
    (mongrel2-server-cert server "key"))

  (:method ((server mongrel2-server) (type string))
    "Return the path of the certificate file of type `type' for `server'"
    (when (= (mongrel2-server-ssl server) 1)
      (make-pathname
       :name (mongrel2-server-uuid server) :type type
       :defaults (merge-pathnames
                  (mongrel2-setting-value (find-mongrel2-setting :certdir))
                  (mongrel2-server-root server))))))

(defmethod mongrel2-server-ssl-p ((server mongrel2-server))
  (if (= (mongrel2-server-ssl server) 0)
      nil
      t))

(defmethod mongrel2-server-pidfile :around ((server mongrel2-server))
  "Wrap the query for a servers pidfile field to return a string
lisp more easily accepts as a relative path"
  (let ((result (call-next-method)))
    (if (eql (char result 0) #\/)
        (subseq result 1)
      result)))

(defmethod mongrel2-host-routes ((host mongrel2-host) &key path)
  (clsql:update-objects-joins `(,host) :force-p t)
  (let ((routes (mongrel2-host-routes-set host)))
    (remove-if-not #'(lambda (route)
                       (or (not path)
                           (equal path (mongrel2-route-path route))))
                   routes)))

(defmethod mongrel2-target-route ((target mongrel2-target))
  "Returns the mongrel2-route instance that binds to this target, or nil.
If any other routes are found they are returned as the second value."
  #.(clsql:locally-enable-sql-reader-syntax)
  (let ((routes (clsql:select 'mongrel2-route :flatp t :refresh t :where
                              [and [= [slot-value 'mongrel2-route 'target-type]
                                      (name-by-endpoint target)]
                                   [= [slot-value 'mongrel2-route 'target-id]
                                      (slot-value target 'id)]])))
    (values (car routes)
            (cdr routes)))
  #.(clsql:locally-disable-sql-reader-syntax))



;;; Model hooks
(defmethod initialize-instance :after ((server mongrel2-server) &rest initargs)
  "Update some instance slots based on other slot values"
  (declare (ignorable initargs))
  (let ((slots '(access-log error-log pid-file)))
    (flet ((update-slot (slot)
             (let ((val (slot-value server slot)))
               (setf (slot-value server slot)
                     (update-template-from-instance server val)))))
      (mapc #'update-slot slots))))

(defmethod initialize-instance :after ((host mongrel2-host) &rest initargs)
  (declare (ignorable initargs))
  (when (and (slot-boundp host 'name)
             (not (slot-boundp host 'matching)))
    (setf (slot-value host 'matching)
          (slot-value host 'name))))

(defmethod clsql-sys::%install-class :after ((view-class (eql (find-class 'mongrel2-mimetype))) db &rest ignore)
  "Load in the known mimetypes when the mimetype table is created"
  (declare (ignorable ignore))
  (clsql:with-transaction ()
    (dolist (mimetype *default-mimetypes* (length *default-mimetypes*))
      (destructuring-bind (ext . type) mimetype
        (clsql:insert-records :into (clsql:view-table view-class)
                              :attributes '(extension mimetype)
                              :values (list ext type))))))

;;; API
(defmethod make-host-route ((host mongrel2-host) path target)
  "Finds or creates a route on `host' and updates it to point to
`target' using `path'"
  (let ((route (or (find-mongrel2-route host path)
                   (make-instance 'mongrel2-route :host-id (model-pk host)
                                  :path path))))
    (setf (mongrel2-route-target route) target)
    (clsql:update-records-from-instance route)
    route))

