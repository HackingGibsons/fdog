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
    (null nil)
    (string (handler-case (read-from-string msg) (end-of-file () nil)))
    (zmq:msg (handler-case (read-from-string (zmq:msg-data-as-string msg)) (end-of-file () nil)))))

(defmethod read-message (sock &key (transform #'zmq:msg-data-as-string) timeout)
  (labels ((s2us (s)
             "Seconds to uSeconds"
             (round (* s 1000000)))

           (fetch (s)
             (let ((msg (make-instance 'zmq:msg)))
               (zmq:recv! s msg)
               msg))
           (wait (s seconds)
             (zmq:with-polls ((poller . ((s . zmq:pollin))))
               (when (zmq:poll poller :timeout (s2us seconds) :retry t)
                 (fetch s)))))

    (let ((data (if timeout
                    (wait sock timeout)
                    (fetch sock))))
      (when data
        (funcall transform data)))))

(defmethod run-program (program args &rest rest)
  "Helper method to run external programs and provide hooks for testing."
  (apply #'sb-ext:run-program program args rest))

(defmethod run-program :around (program args &rest rest)
  "Runs the process and writes the resulting pid to a file"
  (let ((process (call-next-method)))
    (write-pid (sb-ext:process-pid process) program args)
    process))

(defun write-pid (pid program args)
  (format t "~A-" (process-hash program args))
  (format t "~A~%" pid))

(defun process-hash (path args)
  (crypto:byte-array-to-hex-string
   (crypto:digest-sequence :sha256
                           (babel:string-to-octets (format nil "~A ~{~A~^ ~}" path args)))))
