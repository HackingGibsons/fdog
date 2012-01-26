(in-package :request-processing-agent)

(defcategory peer-collection)
(defmethod heard-message ((agent request-processing-agent) (organ agent-requesticle) (from (eql :agent)) (type (eql :info)) &rest info)
  "The requesticle hears an info message. It should examine it to see if contains
any handlers we're interested in and we should connect to them."
  (let* ((info (getf info :info))
         (provides (getf info :provides))
         (servers (getf provides :servers)))
    (labels ((try-connecting (name &key send recv &allow-other-keys)
               "Connect to the given handler if the name matches."
               (log-for (trace requesticle peer-collection) "Maybe connect to: ~S (~A ~A)" name send recv)
               (when (and (string= name (handler-name agent))
                          (not (gethash send (connected-to organ))))
                 (log-for (trace requesticle peer-collection) "Connecting to: ~A => ~A[>~A <~A]" (getf info :uuid) name send recv)
                 (zmq:connect (request-sock organ) send)
                 (zmq:connect (response-sock organ) recv)
                 (setf (gethash send (connected-to organ)) (getf info :uuid))))

             (search-server (server-handlers)
               "Search a server description for handlers"
               (log-for (trace requesticle peer-collection) "Destructuring: ~S" server-handlers)
               (destructuring-bind (server &rest handlers) server-handlers
                 (declare (ignorable server))
                 (log-for (trace requesticle peer-collection) "Handlers: ~S" handlers)
                 (mapc #'(lambda (handler) (apply #'try-connecting handler)) handlers))))

      (log-for (trace requesticle peer-collection) "Requesticle hears info: ~S" info)
      (log-for (trace requesticle peer-collection) "Found servers: ~S" servers)
      (mapc #'search-server servers))))

(defcategory request-handler)
(defmethod request-handler ((agent standard-agent) (organ agent-requesticle) msg)
  "Called with a message read from the `request-socket' of the `organ'.
`agent' is in the arglist for easier specialization."
  (declare (ignorable agent))
  (log-for (trace request-handler) "~A handle request: ~A" organ msg))

(defmethod make-request-handler ((organ agent-requesticle))
  "Construct a callback for `request-socket' in the event
that it is ready for read for submission into the event loop."
  (lambda (sock)
    (request-handler (organ-agent organ) organ
                     (read-message sock :transform #'identity))))

(defmethod reader-callbacks :around ((organ agent-requesticle))
  "Ask to be notified of read activity on the request socket of the `organ'"
  (multiple-value-bind (socks callbacks) (call-next-method)
    (let ((socks (aif (request-sock organ)
                      (append (list it) socks)
                      socks))
          (callbacks (if (request-sock organ)
                         (append (list (make-request-handler organ)) callbacks)
                         callbacks)))
      (values socks
              callbacks))))
