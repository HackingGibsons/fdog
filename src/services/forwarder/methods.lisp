(in-package :fdog-forwarder)

(defmethod interface-start :before ((self fdog-forwarding-interface))
  (log-for (trace) "Starting forwarder ~A" self)
  (init-0mq-endpoints self))

(defmethod interface-stop :after ((self fdog-forwarding-interface))
  (log-for (trace) "Stopping forwarder ~A" self)
  (teardown-0mq-endpoints self))

(defmethod init-0mq-endpoints ((interface fdog-forwarding-interface))
  (log-for (trace) "Building the 0mq context and forwarding sockets.")
  (with-slots (context response-write-sock response-sock request-sock) interface
    (setf context (zmq:init 1)
          request-sock (zmq:socket context zmq:push)
          response-sock (zmq:socket context zmq:sub)
          response-write-sock (zmq:socket context zmq:pub)))
  (log-for (dribble) "Got sockets")
  (describe interface))

(defmethod teardown-0mq-endpoints ((interface fdog-forwarding-interface))
  (log-for (trace) "Tearing down the 0mq context and forwarding interfaces.")
  (with-slots (context response-write-sock response-sock request-sock) interface
    (mapc #'(lambda (sock) (when sock (zmq:close sock)))
          `(,response-write-sock ,response-sock ,request-sock))
    (when context (zmq:term context))

    (setf context nil
          response-write-sock nil
          response-sock nil
          request-sock nil))

  (log-for (dribble) "Teardown complete.")
  (describe interface))

(defmethod initialize-instance :after ((self fdog-forwarding-interface) &rest initargs)
  "Initialize the forwarder"
  (declare (ignorable initargs))
  :undef)

(defun forward-request-handler (handler request raw)
  (declare (ignorable handler raW))
  (log-for (trace) "Request: ~A" request))

(defun mount-forwarder-application (bridge interface)
  (log-for (trace) "Mounting the forwarder application on ~A" bridge)
  (log-for (trace) "Using interface: ~A" interface)
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
