(in-package :afdog-tests)

(defmacro wait-for-agent-message ((uuid &key (timeout 25)) arglist &body forms)
  "Start a conversation with agen at `uuid' with a timeout of `timeout' (25 by default)
and read and parse messages. For each message a lambda constructed as
(lambda `arglist' `forms') will be applied as in (apply #'lambda parsed-message)
If the result of applying the message to the lambda yields a non-`nil' value,
that value is returned. The form blocks for at most `timeout' seconds, when the timeout
is exceeded the return is `nil'"
  (alexandria:with-gensyms (m e msg msg-p result)
    `(flet ((,msg-p ,arglist
              ,@forms))
       (with-agent-conversation (,m ,e :timeout ,timeout) ,uuid
         (do* ((,msg (parse-message (read-message ,m))
                     (parse-message (read-message ,m)))
               (,result (,msg-p ,msg) (,msg-p ,msg)))
              (,result ,result))))))

(defmacro wait-for-agent ((uuid &key (timeout 25)) &body forms)
  "Wait for agent at `uuid' for `timeout' seconds, defaulting to 25.
When any message is first received, `forms' are evaluated and the result
is returned. If `forms' is omitted the result of hearing a message will be
`:found'. If the timeout is reached `nil' is returned."
  (alexandria:with-gensyms (msg)
    `(and (wait-for-agent-message (,uuid :timeout ,timeout) (,msg)
            ,msg)
          (if ',forms
              (progn ,@forms)
              :found))))


(defun http-request-string (resource &key (method :GET) (host "localhost"))
  "Return a string representing an HTTP request for `resource' with `method'
with the Host: header being set to `host' defaulting to \"localhost\""
  (flet ((crlf (stream)
              (write-char #\return stream)
              (write-char #\linefeed stream)))
    (with-output-to-string (s)
      (format s "~A ~A HTTP/1.1" method resource) (crlf s)
      (format s "Host: ~A" host) (crlf s)
      (format s "Accept: */*") (crlf s)
      (format s "User-Agent: Cheap Hack Time") (crlf s)
      (crlf s))))
