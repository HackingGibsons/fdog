(in-package :fdog-forwarder)

(defmethod fdog-forwarder-path ((interface fdog-forwarding-interface))
  (fdog-forwarder-path (forwarder-upstream interface)))

(defmethod interface-start :before ((self fdog-forwarding-interface))
  (log-for (trace) "Starting forwarder ~A" self)
  (init-0mq-endpoints self)
  (start-response-writing-thread self))

(defmethod interface-stop :after ((self fdog-forwarding-interface))
  (log-for (trace) "Stopping forwarder ~A" self)
  (teardown-0mq-endpoints self)
  (stop-response-writing-thread self))

(defmethod start-response-writing-thread ((interface fdog-forwarding-interface))
  (flet ((thread-function ()
           (let ((msg (make-instance 'zmq:msg)))
             (loop while :forever do
                  (log-for (trace) "Waiting for responce message for interface: ~A" interface)
                  (zmq:recv (forwarder-listen-sock interface) msg)
                  (log-for (trace) "I have a message: ~A" (zmq:msg-size msg))
                  (zmq:send (forwarder-response-write-sock interface) msg)
                  (log-for (trace) "I have written it downstream")))))

    (setf (forwarding-interface-response-writer interface)
          (bordeaux-threads:make-thread #'thread-function
                                        :name (format nil "forwarder-response-writer-thread(~A)"
                                                      (fdog-forwarder-name (forwarder-upstream interface)))))))

(defmethod stop-response-writing-thread ((interface fdog-forwarding-interface))
  (with-slots (response-writer) interface
    (and response-writer
         (threadp response-writer)
         (thread-alive-p response-writer)
         (destroy-thread response-writer))
    (setf response-writer nil)))

(defmethod init-0mq-endpoints ((interface fdog-forwarding-interface))
  (log-for (trace) "Building the 0mq context and forwarding sockets.")
  (with-slots (context response-write-sock response-sock request-sock) interface
    ;; Create
    (setf context (zmq:init 1)
          request-sock (zmq:socket context zmq:push)
          response-sock (zmq:socket context zmq:sub)
          response-write-sock (zmq:socket context zmq:pub))


    ;; Connect/Bind
    (with-slots (forward-to listen-on) (forwarder-upstream interface)
      (log-for (trace) "Binding (forward-to) request-sock: ~A" (make-local-endpoint :port forward-to))
      (zmq:setsockopt request-sock zmq:linger 200)
      (zmq:bind request-sock (make-local-endpoint :port forward-to))

      (log-for (trace) "Binding (listen-on) response-sock: ~A" (make-local-endpoint :port listen-on))
      (zmq:setsockopt response-sock zmq:subscribe "")
      (log-for (trace) "Subscribed, binding.")
      (zmq:bind response-sock (make-local-endpoint :port listen-on)))

    (log-for (trace) "Connecting response-write-sock: ~A" (request-handler-pub (interface-bridge-matching interface "/")))
    (zmq:connect response-write-sock (request-handler-pub (interface-bridge-matching interface "/")))))

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

(defun mount-forwarder-application (bridge interface)
  (log-for (trace) "Mounting the forwarder application on ~A" bridge)
  (log-for (trace) "Using interface: ~A" interface)

  (flet ((handler-closure (handler request raw)
           (forward-request-handler handler request raw :interface interface)))
    (setf (request-handler-processors bridge)
          `(,#'handler-closure))))

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
