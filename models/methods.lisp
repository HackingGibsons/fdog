(in-package :fdog-models)

;;; Mongrel2 Server Generic Methods
(defgeneric mongrel2-server-running-p (server)
  (:documentation "Determine if a given server is running or not."))

(defgeneric mongrel2-server-signal (server signal)
  (:documentation ":start :stop :restart :reload :status a given server"))

;;; Method specializations
(defmethod mongrel2-server-signal ((server mongrel2-server) signal)
  (let ((known-signals '(:start :stop :restart :reload :status)))
    (unless (member signal known-signals)
      (error "Signal ~A is unknown. Must be one of ~{~A~^ ~}" signal known-signals)))
  (format t "Will signal ~A server: ~A" server signal)
  'unimp)

(defmethod mongrel2-server-running-p ((server mongrel2-server))
  "T or NIL on (Running or Not-Running given a mongrel2-server
Returns true of it can find a pidfile, and a process is running there."
  (let* ((pidfile (merge-pathnames (mongrel2-server-pidfile server)
                                   (mongrel2-server-root server)))
         (pid (and (probe-file pidfile)
                   (with-open-file (pid pidfile) (read pid)))))
    (and pid
         (eq (kill pid 0) 0))))

;;; Model API Wrappers
(defmethod mongrel2-server-pidfile :around ((server mongrel2-server))
  "Wrap the query for a servers pidfile field to return a string
lisp more easily accepts as a relative path"
  (let ((result (call-next-method)))
    (if (eql (char result 0) #\/)
        (subseq result 1)
      result)))

