(in-package :fdog-forwarder)

;; Multibridge Construction and init
(defmethod make-multibridge ((engine forwarder-engine) (handler mongrel2-handler))
  (log-for (trace) "Building multibridge for: ~A" handler)
  (make-instance 'multibridge :engine engine :handler handler
                 :path (mongrel2-route-path (mongrel2-target-route handler))))

;; Multibridge Operation
(defmethod multibridge-running-bridges ((instance multibridge))
  (remove-if-not #'request-handler-running-p (multibridge-bridges instance)))

(defmethod multibridge-idle-bridges ((instance multibridge))
  (remove-if #'request-handler-running-p (multibridge-bridges instance)))

(defmethod multibridge-start ((instance multibridge))
  (unless (multibridge-bridges instance)
    (log-for (trace) "No bridges present, but asked to start. Adding bridge.")
    (dotimes (x 1)
      (multibridge-add-bridge instance)))
  (mapc #'request-handler-start (multibridge-idle-bridges instance))
  instance)

(defmethod multibridge-stop ((instance multibridge))
  (mapc #'request-handler-stop (multibridge-running-bridges instance))
  instance)

(defmethod multibridge-configure-new-bridge ((instance multibridge) (bridge fdog-handler:request-handler))
  (log-for (trace) "Configuring bridge: ~A" bridge)

  (let ((endpoint (forwarder-engine-endpoint (multibridge-engine instance)))
        (prefix-re (format nil "^~A" (multibridge-path instance))))

    (labels ((rewrite-request (raw)
               (destructuring-bind (sender connection-id path rest)
                   (m2cl::token-parse-n raw 3)

                 (flet ((perform-rewrite ()
                          (let ((json:*json-identifier-name-to-lisp* 'identity)
                                (json:*lisp-identifier-name-to-json* 'identity))
                            (multiple-value-bind (headers-string rest)
                                (m2cl::netstring-parse rest)
                              (let ((headers (json:decode-json-from-string headers-string)))
                                (when (cdr (assoc :PATH headers))
                                  (setf (cdr (assoc :PATH headers))
                                        (ppcre:regex-replace prefix-re (cdr (assoc :PATH headers)) "/")))

                                (when (cdr (assoc :URI headers))
                                  (setf (cdr (assoc :URI headers))
                                        (ppcre:regex-replace prefix-re (cdr (assoc :URI headers)) "/")))

                                (setf headers-string (json:encode-json-to-string headers))

                                (log-for (trace) "Rewriting: ~A ~A" path headers)
                                (flex:string-to-octets (format nil "~A ~A ~A ~A:~A,~A"
                                                               sender connection-id (ppcre:regex-replace prefix-re path "/")
                                                               (length headers-string) headers-string
                                                               (flex:octets-to-string rest))))))))

                   (if (ppcre:scan prefix-re path)
                       (perform-rewrite)
                       raw))))

             (handler-closure (handler request raw)
               (declare (ignorable handler request))
               (with-slots (context request-proxy-addr) endpoint
                 (zmq:with-socket (forward context zmq:push)
                   (maybe-linger-socket forward)
                   (zmq:connect forward request-proxy-addr)
                   (log-for (trace) "Original request: [~A]" (flex:octets-to-string raw))
                   (log-for (trace) "Rewritten request: [~A]" (flex:octets-to-string (rewrite-request raw)))
                   (zmq:send forward (make-instance 'zmq:msg :data (rewrite-request raw)))))))

      (setf (request-handler-processors bridge) `(,#'handler-closure))
      (log-for (trace) "Set request-handler callchain entirely to the forwarder closure.")))

  bridge)

(defmethod multibridge-add-bridge ((instance multibridge))
  (log-for (trace) "Adding bridge to ~A" instance)
  (let ((bridge (configure-bridges-for (multibridge-handler instance))))
    (multibridge-configure-new-bridge instance bridge)
    (push bridge (multibridge-bridges instance))))

(defmethod multibridge-running-p ((instance multibridge))
  (with-slots (bridges) instance
    (and bridges
         (remove-if-not #'request-handler-running-p bridges))))

