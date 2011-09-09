(in-package :afdog)

(defparameter get-local-address (sockets:make-address (sockets:address-to-vector "127.0.0.1")))
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
         (address (car (remove-if-not #'sockets:inet-address-private-p addresses))))
    (and address
         (setf get-local-address address))))
