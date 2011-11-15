(in-package :agent)

(defbehavior announce-self (:interval (:from :heart :nth 3) :do :invoke) (organ)
  (log-for (trace) "Running ~A behavior lambda for ~A" behavior organ)
  (send-message organ :command `(:command :speak
                                 :say (:agent :info :info ,(agent-info (organ-agent organ)))))
  (log-for (trace) "Message sent."))

(defbehavior try-grabbing-public-port (:interval (:from :heart :nth 10) :do :invoke) (organ)
  (let ((addr (format nil "tcp://~A:~A" (get-local-address :update t :as :string) *common-mouth-port*)))
    (ignore-errors
      (zmq:bind (speak-sock organ) addr)
      (log-for (trace) "I seem to have bind'd successfully to ~A" addr))))

(defbehavior speak-when-told (:on (:command :speak :from :head) :do :invoke-with-event) (organ event)
  (let ((message (getf event :say)))
    (send-message organ :raw message :sock (speak-sock organ))
    (log-for (trace) "~A => ~A has spoken: ~A" organ (speak-addr organ) message)))

(defgeneric heard-message (organ from type &rest event)
  (:documentation "A dispatch method for heard messages.")
  (:method ((organ standard-organ) from type &rest event)
    (declare (ignore event))
    (log-for (trace) "~A: Message ~A/~A unhandled." organ from type)))

(defbehavior have-hearing (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message)))
    (apply #'heard-message `(,organ ,@message))))

(defbehavior talk-where-told (:on (:command :speak-to :from :head) :do :invoke-with-event) (organ event)
  (let ((addr (getf event :speak-to)))
    (when (and addr (speak-sock organ)
               (not (gethash addr (speaking-to organ))))

      (setf (gethash addr (speaking-to organ))
            (zmq:connect (speak-sock organ) addr)))))

(defbehavior listen-where-told (:on (:command :listen :from :head) :do :invoke-with-event) (organ event)
  (let ((addr (getf event :listen)))
    (when (and addr (listen-sock organ)
               (not (gethash addr (listening-to organ))))
      (setf (gethash addr (listening-to organ))
            (zmq:connect (listen-sock organ) addr)))))
