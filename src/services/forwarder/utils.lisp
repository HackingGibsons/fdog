(in-package :fdog-forwarder)

(defvar next-handler-port nil)
(defun next-handler-port (&optional (base *handler-zmq-port-base*))
  "Get the next available port to use for a local handler endpoint"
  (let* ((endpoints (loop for handler in (find-mongrel2-handler)
                       appending (list (mongrel2-handler-recv-spec handler)
                                       (mongrel2-handler-send-spec handler))))
         (all-ports (mapcar #'(lambda (a) (car (reverse (ppcre:split ":" a)))) endpoints))
         (all-ports (remove nil (mapcar #'(lambda (h) (parse-integer h :junk-allowed t))
                                        (remove nil all-ports)))))

    (labels ((unused (port)
               (not (member port all-ports)))
             (lowest-free-handler-port (base)
               (loop for port from base to (1- (expt 2 16))
                  if (unused port) return port)))

      (prog1
          (setf next-handler-port
                (lowest-free-handler-port (or next-handler-port base)))
        (incf next-handler-port)))))


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
                           port))

;; Patch in a couple of more commands into redis
;; TODO: Send a pull request: https://github.com/vseloved/cl-redis
(in-package :redis)

(def-cmd WATCH (key) :status "Watch a key.")
(def-cmd UNWATCH () :status "Unwatch any watched keys.")
