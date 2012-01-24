(in-package :afdog-tests)

(defmacro wait-for-agent ((uuid &key (timeout 25)) &body body)
  "Start a conversation with `uuid' and wait to hear a message
timing out in `timeout' and evaluate `body' in a progn as the result.
If `body' is not provided, success is indicated with the value `:found'"
  (alexandria:with-gensyms (m e)
    `(with-agent-conversation (,m ,e :timeout ,timeout) ,uuid
       (declare (ignorable e))
       (and (read-message ,m)
            (if ',body
                (progn ,@body)
                :found)))))

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
