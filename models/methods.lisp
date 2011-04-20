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

;;; Method specializations
(defmethod mongrel2-server-signal ((server mongrel2-server) signal)
  (let ((running (mongrel2-server-running-p server)))
    (ecase signal
      (:start (unless running
                (let ((root (mongrel2-server-root server))
                      (config (mongrel2-server-config server))
                      (uuid (mongrel2-server-uuid server)))
                  (chdir root)
                  (start "mongrel2" `(,config ,uuid)))
                :started))

      (:stop (when running
               (handler-case (progn
                               (kill (mongrel2-server-pid server) sigint) ;; TODO: Accept &optionals to upgrade to sigterm
                               :stopped)
                 (syscall-error () nil))))

      (:restart (mongrel2-server-signal server :stop)
                (mongrel2-server-signal server :start))

      (:reload (when running
                 (handler-case (progn
                                 (kill (mongrel2-server-pid server) sighup)
                                 ;; TODO: Need to send a request to finish the reload
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
    (and running
         (= running 0))))

(defmethod mongrel2-server-pid ((server mongrel2-server))
  (let ((pidfile (merge-pathnames (mongrel2-server-pidfile server)
                                  (mongrel2-server-root server))))
    (when (probe-file pidfile)
      (with-open-file (pid pidfile) (read pid)))))

(defmethod mongrel2-server-config ((server mongrel2-server))
  (merge-pathnames fdog:*default-server-database-path*
                   (mongrel2-server-root server)))

;;; Model API Wrappers
(defmethod mongrel2-server-pidfile :around ((server mongrel2-server))
  "Wrap the query for a servers pidfile field to return a string
lisp more easily accepts as a relative path"
  (let ((result (call-next-method)))
    (if (eql (char result 0) #\/)
        (subseq result 1)
      result)))

