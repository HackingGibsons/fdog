(in-package :afdog-tests)

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
