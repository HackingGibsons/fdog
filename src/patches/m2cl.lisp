(in-package :m2cl)

(export 'request)
(export 'request-parse)

(export 'request-serialize)
(defmethod request-serialize ((request request))
  "Serialize the `request' into an octet array that would represent
the request on the wire."
  (let* ((json:*lisp-identifier-name-to-json* 'identity)
         (headers (json:encode-json-to-string (request-headers request)))
         (body (request-body request))

         (request-header (format nil "~A ~A ~A ~A:~A,~A:"
                                 (request-sender request)
                                 (request-connection-id request)
                                 (request-path request)
                                 (length headers)
                                 headers
                                 (length (request-body request)))))
    (concatenate '(vector (unsigned-byte 8))
                 (babel:string-to-octets request-header)
                 (etypecase body
                   (string (babel:string-to-octets body))
                   ((vector (unsigned-byte 8)) body))
                 (babel:string-to-octets ","))))


;; TODO: These patches need to be sent upsetream, as they are clear bugs
(defmethod handler-receive ((handler handler) &key (timeout -1))
  "Poll the pull socket of HANDLER until there is an available message, read
it, and return the request it contains."
  (zmq:with-poll-items (items nb-items)
                       (((handler-pull-socket handler) :pollin))
    (when (> (zmq:poll items nb-items timeout) 0)
      (when (zmq:poll-item-events-signaled-p (zmq:poll-items-aref items 0)
                                             :pollin)
        (handler-read-request handler)))))
