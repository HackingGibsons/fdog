(in-package :agent)

(defclass agent-ear (standard-beating-organ)
  ((listening-to :initform (make-hash-table :test 'equalp)
                 :accessor listening-to)
   (listen-addr :initform nil
                :accessor listen-addr
                :accessor ear-addr)
   (listen-sock :initform nil
                :accessor listen-sock
                :accessor listen-addr))

  (:documentation "Responsible for delivering incoming inter-agent messages.")
  (:default-initargs . (:tag :ear)))

(defmethod initialize-instance :after ((ear agent-ear) &key)
  (make-listen-where-told ear))

(defmethod agent-boot :after ((agent standard-agent) (ear agent-ear) &rest options)
  "Ear specific socket init."
  (declare (ignorable options))

  (log-for (warn) "Booting ear: ~A => ~A" agent ear)
  (with-slots (listen-addr listen-sock) ear
    (multiple-value-bind (sock addr) (make-local-sock (agent-context agent) zmq:sub)
      (zmq:bind sock (local-ipc-addr ear))
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

(defmethod ear-hear ((ear agent-ear) sock)
  "This method is called when `ear' should read a message from `sock' and act
on it as something 'heard'"
  (let ((message (afdog:parse-message (afdog:read-message sock))))
    (log-for (trace) "~A Heard: [~A]" ear message)
    (when message
      (send-message ear `(,(organ-tag ear) :heard
                           :heard :message
                           :message ,message)))))

(defmethod make-hearer ((ear agent-ear))
  "Return a callback to submit to the poll loop for `ear'"
  #'(lambda (sock) (ear-hear ear sock)))

(defmethod reader-callbacks ((ear agent-ear))
  "The `ear' needs to submit it's earhole at `listen-sock' to the poll loop."
    (values (list (listen-sock ear))
            (list (make-hearer ear))))

