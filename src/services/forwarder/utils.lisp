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

(defmacro used-ports-for-model (model)
  "Collect all of the forward and listen ports of a `fdog-forwarder'-ish model"
  `(reduce #'(lambda (acc next)
               (append acc
                       (list (forwarder-listen-on next) (forwarder-forward-to next))))
           (clsql:select ,model :flatp t :refresh t) :initial-value nil))

(defun all-used-forwarder-ports (&key (order #'>))
  "A full collection of used ports by forwarders and their aliases."
  (sort (append (used-ports-for-model 'fdog-forwarder)
                (used-ports-for-model 'fdog-forwarder-alias))
        order))


(defvar next-forwarder-port nil)
(defun next-forwarder-port (&key reset (inc t))
  "Get the next avialable forwarder port starting at the base and counting up.
If one is not found the next one up from the highest forwarder port in use is used."
  (when reset
    (setf next-forwarder-port (or (car (all-used-forwarder-ports))
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
(def-expect-method :multi
  (let ((n (parse-integer reply)))
    (unless (= n -1)
      (loop :repeat n
         :collect (expect :anything)))))

(def-cmd QSUBSCRIBE (&rest chans) :anything "Subscribe, but accept that the response won't come until after an exec.")
(defmethod tell ((cmd (eql 'QSUBSCRIBE)) &rest args)
  "Wrap QSUBSCRIBE to be subscribe"
  (apply #'tell `(SUBSCRIBE ,@args)))

(def-cmd UNSUBSCRIBE (&rest chans) :unsubscribe
  "Drain the channel messages then return the list of unsubscriptions.")
(defmethod expect ((type (eql :unsubscribe)))
  "Override the unsubscribe return to drain the pending messages
rather than panic and fall down crying. >:["
  (let (unsubs (discarded 0))
    (do ((current (redis:expect :multi) (redis:expect :multi)))
        ((and (string-equal (first current) :unsubscribe)
              (= 0 (parse-integer (car (last current)))))
         (progn (push current unsubs)
                (values unsubs discarded)))
      (if (string-equal (first current) :unsubscribe)
        (push current unsubs)
        (incf discarded)))))

;; Act II:
;; ...In which I rudely invade and ammend a package
(in-package :zmq)

(export 'send!)
(defun send! (sock msg &optional flags (count 0))
  "Keep trying to `zmq:send' while it keeps returning -1 with an errno
of EINTR recursively.  Second value returned is the number of times the operation was retried."
  (let* ((res (zmq:send sock msg flags))
         (res (cond ((and (= res -1)
                          (= (sb-alien:get-errno) sb-posix:eintr))
                     (send! sock msg flags (1+ count)))

                    (:otherwise res))))
    (values res count)))

(export 'recv!)
(defun recv! (sock msg &optional flags (count 0))
  "Keep trying to `zmq:recv' while it keeps returning -1 with an errno
of EINTR or EFSM recursively.  Second value returned is the number of times the operation was retried."
  (let* ((res (zmq:recv sock msg flags))
         (res (cond ((and (= res -1)
                          (member (sb-alien:get-errno) `(,sb-posix:eintr ,zmq:efsm)))
                     (recv! sock msg flags (1+ count)))

                    (:otherwise res))))
    (values res count)))

