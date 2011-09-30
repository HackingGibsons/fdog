(in-package :agent)

(defclass agent-mouth (standard-beating-organ)
  ((speak-addr :initform nil
               :accessor mouth-addr)
   (speak-sock :initform nil
               :accessor mouth-sock))

  (:documentation "Responsible for outgoing cross-agent communication.")
  (:default-initargs . (:tag :mouth)))

(defmethod make-local-sock (context type &key port)
  "Make a locally bound socket of type `type' within `context' using the `transport'
type. Returns two values: the socket created and the address that was bound to in `zmq:connect' format"
  (let ((sock (zmq:socket context type))
        (addr (format nil "tcp://~A:~A" (get-local-address :as :string) (+ 50000 (random 10000)))))
    ;; TODO: ^^ Port generation sucks. Bind and try :O
    (log-for (warn) "TODO: Local sock on ~A" addr)

    (values sock addr)))

(defmethod agent-boot :after ((agent standard-agent) (mouth agent-mouth) &rest options)
  "Mouth specific socket inits."
  (declare (ignorable options))

  (log-for (warn) "Binding info specializer.")
  (defmethod agent-info :around ((agent (eql agent)))
    (let ((info (call-next-method)))
      (log-for (trace) "Got info for ~A: ~A" agent info)
      info))

  (log-for (warn) "TOOD: Booting mouth: ~A from ~A" mouth agent)
  (with-slots (speak-addr speak-sock) mouth
    (multiple-value-bind (sock addr) (make-local-sock (agent-context agent) zmq:pub)
      (log-for (trace) "Setting sock/addr: ~A/~A" sock addr)
      (setf speak-addr addr
            speak-sock sock))))


(defmethod agent-disconnect :after ((agent standard-agent) (mouth agent-mouth) &rest options)
  "Mouth specific disconnect."
  (declare (ignorable options))

  (log-for (warn) "Attempting to remove info specializer.")
  (let ((method (find-method #'agent-info '(:around) `(,(c2mop:intern-eql-specializer agent)) nil)))
    (if method
        (prog1 (remove-method #'agent-info method)
          (log-for (trace) "Removed agent-specific method specialization."))
        (log-for (warn) "Could not find specialization to remove!")))

  (log-for (warn) "Disconnecting mouth: ~A from ~A" mouth agent)
  (with-slots (speak-addr speak-sock) mouth
    (zmq:close speak-sock)
    (setf speak-addr nil
          speak-sock nil)))
