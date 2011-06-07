(in-package :fdog-forwarder)

(defvar next-forwarder-port 50000)
(defun next-forwarder-port (&key reset)
  (when reset
    (setf next-forwarder-port 5000))
  (incf next-forwarder-port))

(defun make-handler-send-spec ()
  (format nil "tcp://127.0.0.1:~A" (next-forwarder-port)))

(defmethod make-handler-send-ident ((forwarder fdog-forwarder))
  (format nil "forwarder-~A" (fdog-forwarder-name forwarder)))

(defun make-handler-recv-spec ()
  (format nil "tcp://127.0.0.1:~A" (next-forwarder-port)))

(defmethod make-handler-recv-ident ((forwarder fdog-forwarder))
  "")
