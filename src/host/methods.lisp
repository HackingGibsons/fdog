(in-package :agent-host)

;; Implementation
(defmethod has-agent-p ((host agent-host) (agent standard-agent))
  (has-agent-p host (agent-uuid agent)))
(defmethod has-agent-p ((host agent-host) (uuid string))
  (or (find uuid (added host) :test #'string-equal :key #'agent-uuid)
      (find uuid (agents host) :test #'string-equal :key #'agent-uuid)))

(defmethod add-agent ((host agent-host)  (agent standard-agent))
  "Add the agent to the container. It will be initialized when the
container is run or during the next tick."
  (unless (find (agent-uuid agent) (added host) :test #'string-equal :key #'agent-uuid)
    (prog1 agent
      (push agent (added host)))))

(defmethod register-agent ((host agent-host) (agent standard-agent))
  "Bind the sockets and context of the `host' to the `agent'.
Once `agent' is added and bound to the transport context a boot event is sent."
  (unless (find (agent-uuid agent) (agents host) :test #'string-equal :key #'agent-uuid)
    ;; TODO: Should there be an agent method for this that is called
    ;;       rather than this being done inline on registration?
    (prog1 agent
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

      (push agent (agents host))
      ;; Send boot
      (agent-publish-event agent `(:boot ,(get-internal-real-time) :uuid ,(agent-uuid agent))))))

(defmethod remove-agent ((host agent-host) (agent standard-agent))
  (remove-agent host (agent-uuid agent)))
(defmethod remove-agent ((host agent-host) (uuid string))
  (pushnew uuid (removed host)))

(defmethod evict-agent ((host agent-host) (agent standard-agent))
  (evict-agent host (agent-uuid agent)))
(defmethod evict-agent ((host agent-host) (uuid string))
  (when-bind agent (find uuid (agents host) :key #'agent-uuid :test #'string-equal)
    (log-for (warn agent-host) "Found agent to remove: ~S" agent)
    (flet ((organ-disconnect (o)
             (log-for (warn agent-host) "Disconnecting: ~A" o)
             (agent-disconnect agent o)))
      (log-for (warn agent-host) "[~A] Disconnecting organs." uuid)
      (mapcar #'organ-disconnect (agent-organs agent))
      (log-for (warn agent-host) "[~A] Organs disconnected." uuid))

    (zmq:close (agent-event-sock agent))
    (zmq:close (agent-message-sock agent))

    (setf (agent-event-sock agent) nil
          (agent-message-sock agent) nil
          (agent-context agent) nil)

    (setf (agents host)
          (delete uuid (agents host) :key #'agent-uuid :test #'string-equal))))

(defmethod run ((host agent-host))
  "Run forever. When finished, returns two values.
The number of loop iterations and the number of events fired."
  (flet ((log-error (c)
           "Write an error report with a stack trace when the run iteration fails."
           (let ((output (make-pathname :directory `(:absolute "tmp") :type "log"
                                        :name (format nil "agent-host.crash.~A-~A" (iolib.syscalls:getpid) (get-universal-time)))))
             (handler-case
                 (with-open-file (s output :if-does-not-exist :create :if-exists :append :direction :output)
                   (format s "~&=Crash report for agent-host ~A: ~A=~%" (iolib.syscalls:getpid)
                           (multiple-value-bind (second minute hour date month year)
                               (decode-universal-time (get-universal-time))
                             (format nil "[~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D]" year month date hour minute second)))
                   (format s "~&==Condition==~%")
                   (format s "~&Type: ~A~%" (type-of c))
                   (trivial-backtrace:print-condition c s)
                   (format s "~&==Condition End==~%")
                   (format s "~&==Backtrace==~%")
                   (trivial-backtrace:print-backtrace-to-stream s)
                   (format s "~&==Backtrace End==~%")
                   (format s "~&=Crash report end=~%")
                   (log-for (warn agent-host) "Crash report written to: ~A" (namestring output)))
               (t (write-error)
                 (log-for (warn agent-host) "Error writing crash report to: ~A: ~A" (namestring output) write-error)))
           (log-for (warn agent-host) "EXITING: Condition raised in iteration: ~S" c))))
    (unwind-protect
         (handler-bind ((error #'log-error))
           (progn
             (setf (running host) t)
             (do ((iter-result (multiple-value-list (run-once host))
                               (multiple-value-list (run-once host))))
                 ((not (agents host))
                  (ticks host)))))
      (setf (running host) nil))))

(defmethod run-once ((host agent-host))
  "Run a single iteration of the event loop for all managed agents.

At the beginning of the call any agents that have not yet been
registered are registered.

Returns three values:
 * The number of callbacks signaled
 * The number of agents managed by the host
 * The number of agents removed this iteration"
  ;; Register one pending added agents
  (when-bind agent (pop (added host))
    (register-agent host agent))

  (let ((callback-agents (make-hash-table :test 'equalp)) ;; Mapping of sockets -> agents for error handling
        (callbacks (make-hash-table :test 'equalp))       ;; Callbacks for sockets firing
        (else-callbacks (make-hash-table :test 'equalp))) ;; Callbacks for sockets that don't fire

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
                            result
                            (remove-agent host agent)))
                 (store-callback-agent agent (agent-event-sock agent))))

             (make-agent-else-callback (agent)
               "Register a callback on the `agent' event socket that fires a `:timeout'
event at the agent. If the `agent' handles the `:timeout' by returning nil it is marked
for removal at the end of the iteration."
               (prog1 #'(lambda ()
                          ;; TODO: Extract this handler-case/bind wrapper to a macro
                          ;;       it exists in `fire-callback' in this method as well
                          (handler-case
                              (handler-bind ((error (curry #'report-error agent)))
                                (if-bind result (handle-agent-event agent :timeout)
                                  (prog1 result (incf (events host)))
                                  (remove-agent host agent)))
                            (t (c)
                              (log-for (warn) "Agent: [~A/~A] Terminating because of: ~A" (type-of agent) (agent-uuid agent) c)
                              (remove-agent host agent))))
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
                                          (log-for (trace agent-host) "Storing callback ~A :out for (~A/~A ~A)" sock (agent-uuid agent) agent organ)
                                          (log-for (trace agent-host) "FD: ~A" (zmq:getsockopt sock :fd))
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
                                          (log-for (trace agent-host) "Storing callback ~A :in for (~A/~A ~A)" sock (agent-uuid agent) agent organ)
                                          (log-for (trace agent-host) "FD: ~A" (zmq:getsockopt sock :fd))
                                          (setf (gethash (sock-id sock) callbacks) fun))
                                      socks funs))))
                        (agent-organs agent))))

             (fire-callback (socket direction)
               "Fire a callback on `socket' in the given `direction' from the callbacks table.
Also removes any `else-callbacks' registered against this callback.
TODO: Handle errors with respect to `callback-agents'"
               (let* ((id (sock-id socket direction))
                      (agent (gethash id callback-agents)))
                 (handler-case
                     (handler-bind ((error (curry #'report-error agent)))
                       (incf (events host))
                       (remhash id else-callbacks)
                       (funcall (gethash id callbacks) socket))
                   (t (c)
                     (log-for (warn) "Agent: [~A/~A] Terminating because of: ~A" (type-of agent) (agent-uuid agent) c)
                     (remove-agent host agent)))))


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
          (log-for (trace agent-host) "Storing callback ~A :in for (~A/~A ~A)" (agent-event-sock agent) (agent-uuid agent) agent :agent)
          (log-for (trace agent-host) "FD: ~A" (zmq:getsockopt (agent-event-sock agent) :fd))
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

            (let ((evicted (prog1 (mapc (curry #'evict-agent host) (removed host))
                             (setf (removed host) (list)))))
              (incf (ticks host))
              (values signaled
                      (length (agents host))
                      (length evicted)))))))))
