(in-package :fdog-forwarder)

(defvar next-forwarder-port nil)
(defun next-forwarder-port (&key reset (inc t))
  (unless next-forwarder-port
    (next-forwarder-port :reset t :inc nil))

  (when reset
    (setf next-forwarder-port (or (loop for forwarder in (clsql:select 'fdog-forwarder :flatp t :update t)
                                     appending `(,(fdog-forwarder-listen-on forwarder)
                                                 ,(fdog-forwarder-forward-to forwarder))
                                       into ports
                                     return (car (sort ports #'>)))
                                  *forwarder-zmq-port-base*)))

  (if inc
      (incf next-forwarder-port)
      next-forwarder-port))

(defun make-handler-send-spec ()
  (format nil "tcp://127.0.0.1:~A" (next-forwarder-port)))

(defmethod make-handler-send-ident ((forwarder fdog-forwarder))
  (format nil "forwarder-~A" (fdog-forwarder-name forwarder)))

(defun make-handler-recv-spec ()
  (format nil "tcp://127.0.0.1:~A" (next-forwarder-port)))

(defmethod make-handler-recv-ident ((forwarder fdog-forwarder))
  "")

(defmethod make-local-endpoint (&key proto addr port)
  (format nil "~A://~A:~A" (or proto "tcp") (or addr (fdog:get-local-address :as :string))
                           (or port (next-forwarder-port))))
