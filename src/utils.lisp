(in-package :afdog)

(defvar *localhost-address* (sockets:make-address (sockets:address-to-vector "127.0.0.1")))
(defparameter get-local-address *localhost-address*)
(defun get-local-address (&key update (as :address))
  "Get the local IP address we should tell clients about.
If `update' is non-nil, it will be recomputed"
  (when (or (not get-local-address) update)
    (setf get-local-address (compute-local-address)))

  (and get-local-address
       (funcall (ecase as
                  (:address #'identity)
                  (:string #'sockets:address-to-string)
                  (:vector #'sockets:address-to-vector))
                get-local-address)))

(defun compute-local-address ()
  "Computethe local IP address"
  (let* ((interfaces (ip-interfaces:get-ip-interfaces))
         (addresses (mapcar #'ip-interfaces:ip-interface-address interfaces))
         (addresses (mapcar #'sockets:make-address addresses))
         (address (car (remove-if-not #'sockets:inet-address-private-p addresses)))
         (address (or address (prog1 *localhost-address* (log-for (warn) "Could not bind to private IP address, falling back to localhost!!")))))
    (and address
         (setf get-local-address address))))

(defmethod make-local-sock (context type &key (linger *socket-linger*))
  "Make a locally bound socket of type `type' within `context' using the `transport'
type. Returns two values: the socket created and the address that was bound to in `zmq:connect' format"
  (flet ((make-addr-string () (format nil "tcp://~A:~A" (get-local-address :update t :as :string) (+ 50000 (random 10000)))))
    (let ((sock (zmq:socket context type))
          addr)
      (zmq:setsockopt sock zmq:linger linger)

      (do ((try-addr (make-addr-string) (make-addr-string)))
          (addr addr)
        (setf addr (handler-case (prog1 try-addr (zmq:bind sock try-addr))
                     (simple-error () nil))))

        (values sock addr))))

(defmethod parse-message (msg)
  (etypecase msg
    (string (handler-case (read-from-string msg) (end-of-file () nil)))
    (zmq:msg (handler-case (read-from-string (zmq:msg-data-as-string msg)) (end-of-file () nil)))))

(defmethod read-message (sock &key (transform #'zmq:msg-data-as-string))
  (let ((msg (make-instance 'zmq:msg)))
    (zmq:recv! sock msg)
    (funcall transform msg)))
