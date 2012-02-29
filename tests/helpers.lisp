(in-package :afdog-tests)

(defmacro wait-for-agent-message ((uuid &key (timeout 25) request requests) arglist &body forms)
  "Start a conversation with agent at `uuid' with a timeout of `timeout' (25 by default).
Optionally sends a `request' at the start of the conversation, then
any `requests' as a list then will read and parse messages.
For each message a lambda constructed as
  (lambda `arglist' `forms')
will be applied as in
  (apply #'lambda parsed-message)
If the result of applying the message to the lambda yields a non-`nil' value,
that value is returned. The form blocks for at most `timeout' seconds, when the timeout
is exceeded the return is `nil'"
  (alexandria:with-gensyms (m e msg msg-p result g!req g!reqs)
    `(flet ((,msg-p ,arglist
              ,@forms))
       (with-agent-conversation (,m ,e :timeout ,timeout) ,uuid
         (let ((,g!req ,request)
               (,g!reqs ,requests))
           (when ,g!req (zmq:send! ,e (prepare-message ,g!req)))
           (dolist (,g!req ,g!reqs)
             (when ,g!req (zmq:send! ,e (prepare-message ,g!req)))))
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

(defun send-message-blindly (uuid &key request)
  "Function to send a message to an agent without waiting on a callback"
  (with-agent-conversation (m e :linger -1) uuid
    (when request (zmq:send! e (prepare-message request)))))


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

(defun http->string (url &key (timeout 10) (method :GET) content-type content)
  "A slightly repackaged veresion of drakma's client.
Mostly exists to close the stream when required and repackage the 7-value
return as something more edible (plist). Though discarding some values that
are less usefull to string construction.
The first value returned is only a string if drakma thinks it is, otherwise
it will be an octet vector"
  (bt:with-timeout (timeout)
    (handler-case
        (multiple-value-bind
              (response status-code headers uri stream must-close reason-phrase)
              (let ((args `(:method ,method :content-type ,content-type :content ,content)))
                (apply 'drakma:http-request `(,url ,@args)))

          (when must-close (close stream))

          (values response
                  (list :status-code status-code
                        :headers headers
                        :uri uri
                        :reason reason-phrase)))

      (bt:timeout () nil))))

(defun http->json (url &rest keys)
  "Wrap `http->string' to parse out JSON, and act more leniently in the face
of octet vectors."
  (multiple-value-bind (res meta) (apply #'http->string `(,url :content-type "application/json" ,@keys))
    (values (typecase res
              (string (json:decode-json-from-string res))
              (vector (json:decode-json-from-string (flex:octets-to-string res)))
              (otherwise res))
            meta)))
