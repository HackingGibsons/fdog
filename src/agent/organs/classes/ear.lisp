(in-package :agent)

(defclass agent-ear (standard-beating-organ)
  ((listen-addr :initform nil
                :accessor listen-addr
                :accessor ear-addr)
   (listen-sock :initform nil
                :accessor listen-sock
                :accessor listen-addr))

  (:documentation "Responsible for delivering incoming inter-agent messages.")
  (:default-initargs . (:tag :ear)))

(defmethod agent-boot :after ((agent standard-agent) (ear agent-ear) &rest options)
  "Ear specific socket init."
  (declare (ignorable options))

  (log-for (warn) "Booting ear: ~A => ~A" agent ear)
  (with-slots (listen-addr listen-sock) ear
    (multiple-value-bind (sock addr) (make-local-sock (agent-context agent) zmq:sub)
      (log-for (warn) "~A now subscribes to everything." ear)
      (zmq:setsockopt sock zmq:subscribe "")

      (setf listen-addr addr
            listen-sock sock))))

(defmethod agent-disconnect :after ((agent standard-agent) (ear agent-ear) &rest options)
  "Disconnecter of the ear"
  (declare (ignorable options))

  (log-for (warn) "Disconnecting ear: ~A from ~A" ear agent)
  (with-slots (listen-addr listen-sock) ear
    (zmq:close listen-sock)
    (setf listen-sock nil
          listen-addr nil)))

(defmethod agent-info ((ear agent-ear))
  `(:ear (:uuid ,(organ-uuid ear) :addr ,(ear-addr ear))))

(defmethod reader-callbacks ((ear agent-ear))
  (flet ((hear-something (sock)
           (let ((msg (make-instance 'zmq:msg)))
             (zmq:recv! sock msg)
             (log-for (trace) "~A Heard: [~A]" ear (zmq:msg-data-as-string msg)))))

    (values (list (listen-sock ear))
            (list #'hear-something))))

