(in-package :fdog-models)

;;; Mongrel2 Server Generic Methods
(defgeneric mongrel2-server-running-p (server)
  (:documentation "Determine if a given server is running or not."))

(defgeneric mongrel2-server-signal (server signal)
  (:documentation ":start :stop :restart :reload :status a given server"))

(defgeneric mongrel2-server-pid (server)
  (:documentation "Get the numeric pid of the server, or nil if it's not running"))

;;; Method specializations
(defmethod mongrel2-server-signal ((server mongrel2-server) signal)
  (let ((running (mongrel2-server-running-p server)))
    (ecase signal
      (:start 'starting)

      (:stop (when running
               (kill (mongrel2-server-pid server) sigint)
               :stopped))

      (:restart (progn
                  (mongrel2-server-signal server :stop)
                  (mongrel2-server-signal server :start)))

      (:reload (when running
                 (kill (mongrel2-server-pid server) sighup)
                 ;; TODO: Need to send a request to finish the reload
                 :reloaded))

      (:status (if running :running :stopped)))))


(defmethod mongrel2-server-running-p ((server mongrel2-server))
  "T or NIL on (Running or Not-Running given a mongrel2-server
Returns true of it can find a pidfile, and a process is running there."
  (let ((pid (mongrel2-server-pid server)))
    (when pid
      (eq (kill pid 0) 0))))

(defmethod mongrel2-server-pid ((server mongrel2-server))
  (let ((pidfile (merge-pathnames (mongrel2-server-pidfile server)
                                  (mongrel2-server-root server))))
    (when (probe-file pidfile)
      (with-open-file (pid pidfile) (read pid)))))

;;; Model API Wrappers
(defmethod mongrel2-server-pidfile :around ((server mongrel2-server))
  "Wrap the query for a servers pidfile field to return a string
lisp more easily accepts as a relative path"
  (let ((result (call-next-method)))
    (if (eql (char result 0) #\/)
        (subseq result 1)
      result)))

