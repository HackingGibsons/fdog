(defpackage #:agent-host
  (:use :cl)
  (:use :log5)
  (:use :afdog)
  (:use :agent)

  (:import-from :arnesi
                :it
                :curry
                :if-bind
                :when-bind
                :awhen)
  (:import-from :alexandria
                :flatten)

  (:export :agent-host))
(in-package :agent-host)

(defcategory agent-host)

(define-condition agent-host-error (error) ())

;; Class and init
(defclass agent-host ()
  ((context :initarg :context
            :initform nil
            :accessor context)
   (agents :initarg :agents
           :initform nil
           :accessor agents))
  (:documentation "A container for running multiple agents in a single event
loop/process."))

(defmethod initialize-instance :after ((inst agent-host) &key)
  "Setup the context and finalizer."
  (let ((ctx (zmq:init 1)))
    (tg:finalize inst #'(lambda () (zmq:term ctx)))
    (setf (context inst) ctx)))

;; Generics
(defgeneric add-agent (host agent)
  (:documentation "Add the agent to the host container."))
(defgeneric remove-agent (host agent)
  (:documentation "Remove and terminate the agent in the host."))
(defgeneric run-once (host)
  (:documentation "Run a single iteration of the event loop and return."))

;; Light helpers
(defun s2us (s)
  "Seconds->uSeconds"
  (round (* s 1000000)))
(defun sock-id (sock &optional (inout :in))
  "Return the pointer address of the sock so we can use it in HTs under `equalp'"
  (format nil "~A-~S" (cffi:pointer-address sock) inout))

;; Implementation
(defmethod add-agent ((host agent-host) (agent standard-agent))
  ;; Bind the sockets and context structures
  ;; TODO: Extract
  (setf (agent-context agent) (context host)
        (agent-event-sock agent) (zmq:socket (context host) :sub)
        (agent-message-sock agent) (zmq:socket (context host) :pub))

  ;; Bind the sockets
  (zmq:bind (agent-event-sock agent) (agent-event-addr agent))
  (zmq:bind (agent-message-sock agent) (agent-message-sock agent))

  ;; Set options
  (zmq:setsockopt (agent-event-sock agent) :linger *socket-linger*)
  (zmq:setsockopt (agent-event-sock agent) :subscribe "")
  (zmq:setsockopt (agent-message-sock agent) :linger *socket-linger*)



  (when (not (find agent (agents host) :test #'string-equal :key #'agent-uuid))
    (prog1 agent
      (push agent (agents host))
      ;; Send boot
      (agent-publish-event agent `(:boot ,(get-internal-real-time) :uuid ,(agent-uuid agent))))))

(defmethod remove-agent ((host agent-host) (agent standard-agent))
  (remove-agent host (agent-uuid agent)))
(defmethod remove-agent ((host agent-host) (uuid string))
  (when-bind agent (find uuid (agents host) :key #'agent-uuid :test #'equalp)
    (flet ((organ-disconnect (o) (agent-disconnect agent o)))
      (log-for (warn) "[~A] Disconnecting organs." uuid)
      (mapcar #'organ-disconnect (agent-organs agent))
      (log-for (warn) "[~A] Organs disconnected." uuid))
    (delete uuid (agents host) :key #'agent-uuid :test #'equalp)))


(defmethod run-once ((host agent-host))
  (let ((callbacks (make-hash-table :test 'equalp))       ;; Callbacks for sockets firing
        (else-callbacks (make-hash-table :test 'equalp))  ;; Callbacks for sockets that don't fire
        (remove (list)))                                  ;; List of UUIDs of agents that should be removed

    (labels ((make-agent-event-callback (agent)
               #'(lambda (sock)
                   (if-bind result (handle-agent-event agent (read-message sock))
                     result
                     (pushnew (agent-uuid agent) remove :test #'string-equal))))

             (make-agent-else-callback (agent)
               #'(lambda ()
                   (if-bind result (handle-agent-event agent :timeout)
                     result
                     (pushnew (agent-uuid agent) remove :test #'string-equal))))

             (organ-writers+store-callbacks (agent)
               "Returns a list of writer sockets and fills in the callbacks for them in the HT"
               (alexandria:flatten
                (mapcar #'(lambda (organ)
                            (multiple-value-bind (socks funs) (writer-callbacks organ)
                              (prog1 socks
                                (mapc #'(lambda (sock fun)
                                          (setf (gethash (sock-id sock :out) callbacks) fun))
                                      socks funs))))
                        (agent-organs agent))))

             (organ-readers+store-callbacks (agent)
               "Return a list of organ reader sockets and fill in the callbacks in the HT"
               (alexandria:flatten
                (mapcar #'(lambda (organ)
                            (multiple-value-bind (socks funs) (reader-callbacks organ)
                              (prog1 socks
                                (mapc #'(lambda (sock fun)
                                          (setf (gethash (sock-id sock) callbacks) fun))
                                      socks funs))))
                        (agent-organs agent)))))

      (let ((timeout (s2us (apply #'min (or (mapcar #'agent-poll-timeout (agents host))
                                            `(0)))))
            (readers (alexandria:flatten (mapcar #'organ-readers+store-callbacks (agents host))))
            (writers (alexandria:flatten (mapcar #'organ-writers+store-callbacks (agents host)))))

        (mapc #'(lambda (agent)
                  ;; Agent event callback
                  (appendf readers (agent-event-sock agent))
                  (setf (gethash (sock-id (agent-event-sock agent)) callbacks)
                        (make-agent-event-callback agent))
                  ;; Agent lack of event callback
                  (setf (gethash (sock-id (agent-event-sock agent)) else-callbacks)
                        (make-agent-else-callback agent)))
              (agents host))

        (zmq:with-poll-sockets (items nb-items :in readers :out writers)
          (let ((signaled (zmq:poll items nb-items timeout)))
            (when (> signaled 0)
              (zmq:do-poll-items (item items nb-items)
                (awhen (zmq:poll-item-events-signaled-p item :pollin)
                  (remhash (sock-id (zmq:poll-item-socket item) :in) else-callbacks)
                  (funcall (gethash (sock-id (zmq:poll-item-socket item) :in) callbacks)
                           (zmq:poll-item-socket item)))
                (awhen (zmq:poll-item-events-signaled-p item :pollout)
                  (remhash (sock-id (zmq:poll-item-socket item) :out) else-callbacks)
                  (funcall (gethash (sock-id (zmq:poll-item-socket item) :out) callbacks)
                           (zmq:poll-item-socket item)))))

            (maphash #'(lambda (key val)
                         (log-for (warn agent-host) "TODO: Calling else callback: ~S => ~S" key val)
                         (funcall val))
                     else-callbacks)
            (mapc (curry #'remove-agent host) remove)

            (values signaled
                    (length remove))))))))




