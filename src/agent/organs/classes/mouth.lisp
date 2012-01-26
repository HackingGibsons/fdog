(in-package :agent)

(export '*common-mouth-port*)
(defvar *common-mouth-port* 10101
  "The common port that every agent tries to grab to broadcast network information
out of.")

(defclass agent-mouth (standard-beating-organ)
  ((speaking-to :initform (make-hash-table :test 'equalp)
                :accessor speaking-to)
   (speak-addr :initform nil
               :accessor speak-addr
               :accessor mouth-addr)
   (speak-sock :initform nil
               :accessor speak-sock
               :accessor mouth-sock))

  (:documentation "Responsible for outgoing cross-agent communication.")
  (:default-initargs . (:tag :mouth)))

(defmethod initialize-instance :after ((mouth agent-mouth) &key)
  (make-try-grabbing-public-port mouth)
  (make-talk-where-told mouth)
  (make-speak-when-told mouth))

(defmethod agent-boot :after ((agent standard-agent) (mouth agent-mouth) &rest options)
  "Mouth specific socket inits."
  (declare (ignorable options))

  (log-for (warn) "Booting mouth: ~A from ~A" mouth agent)
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
    (zmq:setsockopt speak-sock zmq:linger 3000)
    (when speak-sock
      (zmq:close speak-sock))
    (setf speak-addr nil
          speak-sock nil)))


(defmethod agent-info ((mouth agent-mouth))
  (log-for (trace) "~A DID define additional info." mouth)
  `(:mouth (:uuid ,(organ-uuid mouth) :addr ,(mouth-addr mouth))))
