(in-package :agent-host)

;; Implementation
(defmethod add-agent ((host agent-host)  (agent standard-agent))
  "Add the agent to the container. It will be initialized when the
container is run or during the next tick."
  (unless (find (agent-uuid agent) (added host) :test #'string-equal :key #'agent-uuid)
    (prog1 agent
      (push agent (added host)))))

(defmethod register-agent ((host agent-host) (agent standard-agent))
  ;; Bind the sockets and context structures
  ;; TODO: Extract
  (setf (agent-context agent) (context host)
        (agent-event-sock agent) (zmq:socket (context host) :sub)
        (agent-message-sock agent) (zmq:socket (context host) :pub))

  ;; Bind the sockets
  (zmq:bind (agent-event-sock agent) (agent-event-addr agent))
  (zmq:bind (agent-message-sock agent) (agent-message-addr agent))

  ;; Set options
  (zmq:setsockopt (agent-event-sock agent) :linger *socket-linger*)
  (zmq:setsockopt (agent-event-sock agent) :subscribe "")
  (zmq:setsockopt (agent-message-sock agent) :linger *socket-linger*)

  (unless (find (agent-uuid agent) (agents host) :test #'string-equal :key #'agent-uuid)
    (prog1 agent
      (push agent (agents host))
      ;; Send boot
      (agent-publish-event agent `(:boot ,(get-internal-real-time) :uuid ,(agent-uuid agent))))))

(defmethod remove-agent ((host agent-host) (agent standard-agent))
  (remove-agent host (agent-uuid agent)))
(defmethod remove-agent ((host agent-host) (uuid string))
  (when-bind agent (find uuid (agents host) :key #'agent-uuid :test #'string-equal)
    (log-for (warn agent-host) "Found agent to remove: ~S" agent)
    (flet ((organ-disconnect (o) (agent-disconnect agent o)))
      (log-for (warn agent-host) "[~A] Disconnecting organs." uuid)
      (mapcar #'organ-disconnect (agent-organs agent))
      (log-for (warn agent-host) "[~A] Organs disconnected." uuid))
    (zmq:close (agent-event-sock agent))
    (zmq:close (agent-message-sock agent))
    (setf (agents host)
          (delete uuid (agents host) :key #'agent-uuid :test #'string-equal))))

(defmethod run ((host agent-host))
  "Run forever. When finished, returns two values.
The number of loop iterations and the number of events fired."
  (do ((iter-result (multiple-value-list (run-once host))
                    (multiple-value-list (run-once host))))
       ((not (agents host))
        (ticks host))))

(defmethod run-once ((host agent-host))
  "Run a single iteration of the event loop for all managed agents.

At the beginning of the call any agents that have not yet been
registered are registered.

Returns three values:
 * The number of callbacks signaled
 * The number of agents managed by the host
 * The number of agents removed this iteration"
  ;; Register any pending added agents
  (do ((agent (pop (added host)) (pop (added host))))
      ((not agent))
    (register-agent host agent))

  (let ((callback-agents (make-hash-table :test 'equalp)) ;; Mapping of sockets -> agents for error handling
        (callbacks (make-hash-table :test 'equalp))       ;; Callbacks for sockets firing
        (else-callbacks (make-hash-table :test 'equalp))  ;; Callbacks for sockets that don't fire
        (remove (list)))                                  ;; List of UUIDs of agents that should be removed

    (labels ((store-callback-agent (agent sock &optional (direction :in))
               "Store which `agent' requested the binding of a callback on `sock' in `direction'
Used to trap errors with the correct agent around callback invocation."
               (setf (gethash (sock-id sock direction) callback-agents) agent))

             (make-agent-event-callback (agent)
               "Make a callback that will read and process an agent event. If the agent
fails to successfully process the message by returning `nil' the agent is marked
for removal at the end of the iteration."
               (prog1 #'(lambda (sock)
                          (if-bind result (handle-agent-event agent (read-message sock))
                            (prog1 result (incf (events host)))
                            (pushnew (agent-uuid agent) remove :test #'string-equal)))
                 (store-callback-agent agent (agent-event-sock agent))))

             (make-agent-else-callback (agent)
               "Register a callback on the `agent' event socket that fires a `:timeout'
event at the agent. If the `agent' handles the `:timeout' by returning nil it is marked
for removal at the end of the iteration."
               (prog1 #'(lambda ()
                          (if-bind result (handle-agent-event agent :timeout)
                            (prog1 result (incf (events host)))
                            (pushnew (agent-uuid agent) remove :test #'string-equal)))
                 (store-callback-agent agent (agent-event-sock agent))))


             (organ-writers+store-callbacks (agent)
               "Returns a list of writer sockets of `agent' and fills in the callbacks for them in the HT
along with the reference to the given `agent' in the ownership table."
               (alexandria:flatten
                (mapcar #'(lambda (organ)
                            (multiple-value-bind (socks funs) (writer-callbacks organ)
                              (prog1 socks
                                (mapc #'(lambda (sock fun)
                                          (store-callback-agent agent sock :out)
                                          (setf (gethash (sock-id sock :out) callbacks) fun))
                                      socks funs))))
                        (agent-organs agent))))

             (organ-readers+store-callbacks (agent)
               "Return a list of organ reader sockets and fill in the callbacks in the HT
along with the reference to the given `agent' in the ownership table."
               (alexandria:flatten
                (mapcar #'(lambda (organ)
                            (multiple-value-bind (socks funs) (reader-callbacks organ)
                              (prog1 socks
                                (mapc #'(lambda (sock fun)
                                          (store-callback-agent agent sock :in)
                                          (setf (gethash (sock-id sock) callbacks) fun))
                                      socks funs))))
                        (agent-organs agent))))

             (fire-callback (socket direction)
               "Fire a callback on `socket' in the given `direction' from the callbacks table.
Also removes any `else-callbacks' registered against this callback.
TODO: Handle errors with respect to `callback-agents'"
               (let ((id (sock-id socket direction)))
                 (incf (events host))
                 (remhash id else-callbacks)
                 (funcall (gethash id callbacks) socket)))

             (maybe-trigger-callback (pollitem)
               "Try to trigger a callback from the hash table if this pollitem is signaled for IO"
               (when (zmq:poll-item-events-signaled-p pollitem :pollin)
                 (fire-callback (zmq:poll-item-socket pollitem) :in))
               (when (zmq:poll-item-events-signaled-p pollitem :pollout)
                 (fire-callback (zmq:poll-item-socket pollitem) :out))))

      (let ((timeout (s2us (apply #'min (or (mapcar #'agent-poll-timeout (agents host))
                                            `(0)))))
            ;; These bind only the organ socket callbacks from each agent
            (readers (alexandria:flatten (mapcar #'organ-readers+store-callbacks (agents host))))
            (writers (alexandria:flatten (mapcar #'organ-writers+store-callbacks (agents host)))))

        ;; Here we bind all of the agent-side event sockets
        ;; And bind a failure callback for them so agents can
        ;; Receive :timeout messages when they fail to read an event
        ;; TODO: That operation primarily drives the agent :beat clock
        ;;       and should be the first for extraction when we use
        ;;       and external loop like libev
        (dolist (agent (agents host))
          (appendf readers (list (agent-event-sock agent)))
          (setf (gethash (sock-id (agent-event-sock agent)) callbacks)
                (make-agent-event-callback agent)

                (gethash (sock-id (agent-event-sock agent)) else-callbacks)
                (make-agent-else-callback agent)))

        (zmq:with-poll-sockets (items nb-items :in readers :out writers)
          (let ((signaled (zmq:poll items nb-items timeout)))
            (when (> signaled 0)
              (zmq:do-poll-items (item items nb-items)
                (maybe-trigger-callback item)))

            (maphash #'(lambda (key val)
                         (declare (ignore key))
                         (funcall val))
                     else-callbacks)
            (mapc (curry #'remove-agent host) remove)

            (incf (ticks host))

            (values signaled
                    (length (agents host))
                    (length remove))))))))
