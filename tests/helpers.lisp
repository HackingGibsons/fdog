(in-package :afdog-tests)

(defmacro with-agent-conversation ((mouth-binding ear-binding &key (timeout 25) (linger 250)) uuid &body forms)
  "Execute `forms' with `mouth-binding' and `ear-binding' bound to connected sockets
to the ear and mouth of an agent with the `uuid' given using the local transport.
The `forms' must execute within `timeout' (15 by default) or nil is returned instead of the
result."
  (let ((e!agent (gensym "AGENT"))
        (e!linger (gensym "LINGER"))
        (g!context (gensym "CONTEXT"))
        (g!result (gensym "RESULT")))
    `(let ((,e!agent ,uuid)
           (,e!linger ,linger)
           ,g!result)
       (handler-case (bt:with-timeout (,timeout)
                       (zmq:with-context (,g!context 1)
                         (zmq:with-socket (,mouth-binding ,g!context zmq:sub)
                           (zmq:with-socket (,ear-binding ,g!context zmq:pub)
                             (zmq:setsockopt ,mouth-binding zmq:linger ,e!linger)
                             (zmq:setsockopt ,ear-binding zmq:linger ,e!linger)
                             (zmq:connect ,mouth-binding (agent::local-ipc-addr ,e!agent :mouth))
                             (zmq:connect ,ear-binding (agent::local-ipc-addr ,e!agent :ear))
                             (zmq:setsockopt ,mouth-binding zmq:subscribe "")
                             (setf ,g!result (progn ,@forms))))))
         (bt:timeout ()
           (log-for (warn) "Agent ~A conversation timing out!" ,e!agent)
           ;; TODO: Keep the log, toss the format when logging is better :|
           (format t "[WARN] Agent ~A conversation timing out!~%" ,e!agent)
           nil))
       ,g!result)))
