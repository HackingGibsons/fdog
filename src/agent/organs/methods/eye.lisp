(in-package :agent)

(defmethod initialize-instance :after ((eye agent-eye) &key)
  (make-look-when-told eye))

(defgeneric see (organ subject &rest args)
  (:documentation "the act of looking at something"))

(defmethod see (organ (subject (eql :process)) &rest info)
  (log-for (trace eye organ) "~A is looking at ~A and notices ~A" organ subject info)
  (let* ((pid (getf info :pid))
         (running-p (handler-case (zerop (iolib.syscalls:kill pid 0))
                      (iolib.syscalls:syscall-error () nil)))
         (message `(:saw :pid :pid ,pid :alive ,(if running-p t nil))))
    (log-for (trace eye organ) "Sending message ~A" message)
    (send-message organ :saw message)))
