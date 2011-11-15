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
