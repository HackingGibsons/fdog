(in-package :fdog-forwarder)

(defvar next-forwarder-port nil)
(defun next-forwarder-port (&key reset (inc t))
  "Get the next avialable forwarder port starting at the base and counting up.
If one is not found the next one up from the highest forwarder port in use is used."
  (when reset
    (setf next-forwarder-port (or (loop for forwarder in (clsql:select 'fdog-forwarder :flatp t :refresh t)
                                     appending `(,(fdog-forwarder-listen-on forwarder)
                                                 ,(fdog-forwarder-forward-to forwarder))
                                       into ports
                                     return (car (sort ports #'>)))
                                  *forwarder-zmq-port-base*)))
  (unless next-forwarder-port
    (next-forwarder-port :reset t :inc nil))
  (if inc
      (incf next-forwarder-port)
      next-forwarder-port))

(defmethod make-local-endpoint (&key proto addr port)
  "Constructs a zeromq endpoint using the local address. &key `port' is required,
remainder have sane defaults"
  (unless port (error "Port required, can't make sane default"))
  (format nil "~A://~A:~A" (or proto "tcp") (or addr (fdog:get-local-address :as :string))
                           (or port (next-forwarder-port))))
