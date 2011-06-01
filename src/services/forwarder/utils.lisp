(in-package :fdog-forwarder)

(defun make-handler-send-spec ()
  "tcp://127.0.0.1:59910")

(defmethod make-handler-send-ident ((forwarder fdog-forwarder))
  (format nil "forwarder-~A" (fdog-forwarder-name forwarder)))

(defun make-handler-recv-spec ()
  "tcp://127.0.0.1:29999")

(defmethod make-handler-recv-ident ((forwarder fdog-forwarder))
  "")
