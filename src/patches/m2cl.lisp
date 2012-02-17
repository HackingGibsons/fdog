(in-package :m2cl)

(export 'request)

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

