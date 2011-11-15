(in-package :agent)

(defmacro with-agent-conversation ((mouth-binding ear-binding &key (timeout 25) (linger 250)) uuid-or-many &body forms)
  "Execute `forms' with `mouth-binding' and `ear-binding' bound to connected sockets
to the ear and mouth of an agent or agents with the `uuid-or-many' UUIDs given using the local transport.
`uuid-or-many' can either be a single UUID string or a list of them. The sockets will connect
to agents named by every `uuid-or-many' received.
The `forms' must execute within `timeout' (15 by default) or nil is returned instead of the
result."
  (let ((e!agents (gensym "AGENT"))
        (e!linger (gensym "LINGER"))
        (g!context (gensym "CONTEXT"))
        (g!result (gensym "RESULT")))
    `(let* ((,e!agents ,uuid-or-many)
            (,e!agents (if (listp ,e!agents)
                           ,e!agents
                           (list ,e!agents)))
           (,e!linger ,linger)
           ,g!result)
       (handler-case (bt:with-timeout (,timeout)
                       (zmq:with-context (,g!context 1)
                         (zmq:with-socket (,mouth-binding ,g!context zmq:sub)
                           (zmq:with-socket (,ear-binding ,g!context zmq:pub)
                             (zmq:setsockopt ,mouth-binding zmq:linger ,e!linger)
                             (zmq:setsockopt ,ear-binding zmq:linger ,e!linger)
                             (mapc #'(lambda (uuid)
                                       (zmq:connect ,mouth-binding (local-ipc-addr uuid :mouth))
                                       (zmq:connect ,ear-binding (local-ipc-addr uuid :ear)))
                                   ,e!agents)
                             (zmq:setsockopt ,mouth-binding zmq:subscribe "")
                             (setf ,g!result (progn ,@forms))))))
         (bt:timeout ()
           (log-for (warn) "Agent ~A conversation timing out!" ,e!agents)
           ;; TODO: Keep the log, toss the format when logging is better :|
           (format t "[WARN] Agent ~A conversation timing out!~%" ,e!agents)
           nil))
       ,g!result)))

(defcategory discover-agents)
(defmacro discover-agents-on-host ((&key (host "127.0.0.1") (timeout 25)) arglist &body forms)
  "Look for an agent's public mouth on the host `host' for `timeout'
Each time an agent info message arrives the `forms' will be evaluated with bindings as in the arglist
invoked as in (flet ((f arglist forms)) (f uuid info-plist))
If the `forms' evaluate to non-nil, the form terminates returning the result.
If the timeout is reached nil is returned."
  (let ((g!context (gensym "CONTEXT"))
        (g!seek-sock (gensym "SEEK-SOCK"))
        (g!msg (gensym "MSG"))
        (g!info (gensym "INFO"))
        (g!uuid (gensym "UUID"))
        (g!process-agent (gensym "PROCESS-AGENT"))
        (g!result (gensym "RESULT"))

        (e!host (gensym "HOST")))
    `(let ((,e!host ,host))
       (labels ((,g!process-agent ,arglist ,@forms)
                (discover-agents ()
                  (zmq:with-context (,g!context 1)
                    (zmq:with-socket (,g!seek-sock ,g!context zmq:sub)
                      (zmq:connect ,g!seek-sock (format nil "tcp://~A:~A" ,e!host agent::*common-mouth-port*))
                      (zmq:setsockopt ,g!seek-sock zmq:subscribe "")

                      (do* ((,g!msg (parse-message (read-message ,g!seek-sock))
                                    (parse-message (read-message ,g!seek-sock)))
                            (,g!info (and (getf ,g!msg :agent) (getf ,g!msg :info))
                                     (and (getf ,g!msg :agent) (getf ,g!msg :info)))
                            (,g!uuid (getf ,g!info :uuid)
                                     (getf ,g!info :uuid))
                            (,g!result (and ,g!info ,g!uuid
                                           (,g!process-agent ,g!uuid ,g!info))))
                           (,g!result ,g!result))))))

         (handler-case (bt:with-timeout (,timeout) (discover-agents))
           (bt:timeout () nil))))))
