(defpackage #:agent-host
  (:use :cl)
  (:use :log5)
  (:use :afdog)
  (:use :agent)

  (:import-from :arnesi
                :it
                :awhen)

  (:export :agent-host))
(in-package :agent-host)

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
  (setf (agent-context agent) (context host))
  (pushnew (agents host) agent :test #'string-equal :key #'agent-uuid))

(defmethod run-once ((host agent-host))
  (let ((callbacks (make-hash-table :test 'equalp))       ;; Callbacks for sockets firing
        (else-callbacks (make-hash-table :test 'equalp))) ;; Callbacks for sockets that don't fire

    (let ((timeout 0)
          (readers (list))
          (writers (list)))

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
                         (zmq:poll-item-socket item))))))))))




