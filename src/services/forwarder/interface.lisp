(in-package :fdog-forwarder)

(defclass fdog-forwarding-interface (fdog-interface)
  ((upstream :initarg :upstream
             :accessor forwarder-upstream)
   (context :initform nil
            :initarg :context
            :accessor forwarder-context)
   (response-write-sock :initform nil
                        :initarg :response-write-sock
                        :accessor forwarder-response-write-sock)
   (response-sock :initform nil
                :initarg :listen-sock
                :accessor forwarder-listen-sock)
   (request-sock :initform nil
                 :initarg :request-sock
                 :accessor forwarder-request-sock)
   (response-writer :initform nil
                    :accessor forwarding-interface-response-writer))
  (:documentation "An interface for forwarding requests to upstream 0mq endpoints."))

(defmethod interface-start :before ((self fdog-forwarding-interface))
  (log-for (trace) "Starting forwarder ~A" self)
  (init-0mq-endpoints self))

(defmethod interface-stop :after ((self fdog-forwarding-interface))
  (log-for (trace) "Stopping forwarder ~A" self)
  (teardown-0mq-endpoints self))

(defmethod init-0mq-endpoints ((interface fdog-forwarding-interface))
  (log-for (trace) "Building the 0mq context and forwarding sockets."))

(defmethod teardown-0mq-endpoints ((interface fdog-forwarding-interface))
  (log-for (trace) "Tearing down the 0mq context and forwarding interfaces."))

(defmethod initialize-instance :after ((self fdog-forwarding-interface) &rest initargs)
  "Initialize the forwarder"
  (declare (ignorable initargs))
  :undef)

(defun forward-request-handler (handler request raw)
  (log-for (trace) "Request: ~A" request))

(defun mount-forwarder-application (bridge)
  (log-for (trace) "Mounting the forwarder application on ~A" bridge)
  (setf (request-handler-processors bridge)
        '(forward-request-handler))
  (describe bridge)
  :undef)

(defmethod make-forwarder-interface ((forwarder fdog-forwarder))
  (log-for (trace) "Making interface from forwarder ~A" forwarder)
  (let* ((server (ensure-server-exists forwarder *forwarder-server-name* *forwarder-server-port*))
         (host (mongrel2-server-default-host server))
         (handler (ensure-handler-for-forwarder forwarder :host host :server server))
         (interface (make-instance 'fdog-forwarding-interface
                                   :server server
                                   :upstream forwarder)))
    (declare (ignore handler))

    (interface-configure-bridges (interface)
      ("/" :mount-bridge 'mount-forwarder-application))

    interface))


(defmethod init-forwarders (&key (start t))
  "Search for, init and start all known forwarders"
  (log-for (dribble) "Initializing forwarders..")
  (when (not (clsql:table-exists-p (clsql:view-table (find-class 'fdog-forwarder))))
    (log-for (trace) "Forwarder table does not exist.. creating.")
    (clsql:create-view-from-class 'fdog-forwarder))

  ;; Turn off any that we already have running
  (log-for (trace) "Removing current forwarders..")
  (setf *forwarders*
        (dolist (forwarder *forwarders* nil)
          (interface-stop forwarder)
          (log-for (trace) "Removed: ~A" forwarder)))

  (log-for (trace) "Adding forwarders..")
  (setf *forwarders*
        (mapcar #'make-forwarder-interface
                (clsql:select 'fdog-forwarder :flatp t :refresh t)))

  (when start
    (log-for (trace) "Starting forwarders..")
    (mapcar #'interface-start *forwarders*))

  *forwarders*)
