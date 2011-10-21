(in-package :agent)

(defclass agent-mouth (standard-beating-organ)
  ((speak-addr :initform nil
               :accessor speak-addr
               :accessor mouth-addr)
   (speak-sock :initform nil
               :accessor speak-sock
               :accessor mouth-sock))

  (:documentation "Responsible for outgoing cross-agent communication.")
  (:default-initargs . (:tag :mouth)))

(defmethod initialize-instance :after ((mouth agent-mouth) &key)
  (make-speak-when-told mouth))

(defmethod agent-boot :after ((agent standard-agent) (mouth agent-mouth) &rest options)
  "Mouth specific socket inits."
  (declare (ignorable options))

  (log-for (warn) "TOOD: Booting mouth: ~A from ~A" mouth agent)
  (with-slots (speak-addr speak-sock) mouth
    (multiple-value-bind (sock addr) (make-local-sock (agent-context agent) zmq:pub)
      (zmq:bind sock (local-ipc-addr mouth))
      (log-for (trace) "Setting sock/addr: ~A/~A" sock addr)
      (setf speak-addr addr
            speak-sock sock))))


(defmethod agent-disconnect :after ((agent standard-agent) (mouth agent-mouth) &rest options)
  "Mouth specific disconnect."
  (declare (ignorable options))

  (log-for (warn) "Disconnecting mouth: ~A from ~A" mouth agent)
  (with-slots (speak-addr speak-sock) mouth
    (zmq:close speak-sock)
    (setf speak-addr nil
          speak-sock nil)))


(defmethod agent-info ((mouth agent-mouth))
  (log-for (trace) "~A DID define additional info." mouth)
  `(:mouth (:uuid ,(organ-uuid mouth) :addr ,(mouth-addr mouth))))
