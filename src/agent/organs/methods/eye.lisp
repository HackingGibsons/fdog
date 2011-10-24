(in-package :agent)

(defmethod initialize-instance :after ((eye agent-eye) &key)
  (make-look-when-told eye)
  (make-watch-when-told eye))

(defgeneric see (organ subject &rest args)
  (:documentation "the act of looking at something"))

(defmethod see ((organ standard-organ) (subject (eql :agent)) &rest info)
  (log-for (trace eye organ) "~A is looking at agent ~A and notices ~A" organ subject info)
  (let* ((head (find-organ (organ-agent organ) :head))
         (uuid (getf info :uuid))
         (peer (and head uuid (gethash uuid (agent-peers head)))))
    (send-message organ :saw
                  `(:saw :agent
                    :agent (:uuid ,uuid :info ,peer)))))


(defmethod see ((organ standard-organ) (subject (eql :process)) &rest info)
  (log-for (trace eye organ) "~A is looking at ~A and notices ~A" organ subject info)
  (let* ((pid (getf info :pid))
         (running-p (and (numberp pid)
                         (handler-case (zerop (iolib.syscalls:kill pid 0))
                           (iolib.syscalls:syscall-error () nil))))
         (message `(:saw :process :process (:pid ,pid :alive ,(if running-p t nil)))))
    (log-for (trace eye organ) "Sending message ~A" message)
    (send-message organ :saw message)))
