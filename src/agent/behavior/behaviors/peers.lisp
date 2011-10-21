(in-package :agent)

(defbehavior announce-self (:interval (:from :heart :nth 3) :do :invoke) (organ)
  (log-for (trace) "Running ~A behavior lambda for ~A" behavior organ)
  (send-message organ `(,(organ-tag organ) :command
                         :command :speak
                         :say (:agent :info :info ,(agent-info (organ-agent organ)))))
  (log-for (trace) "Message sent."))

(defbehavior speak-when-told (:on (:command :speak :from :head) :do :invoke-with-event) (organ event)
  (let ((message (getf event :say)))
    (send-message organ message :sock (speak-sock organ))
    (log-for (trace) "~A => ~A has spoken: ~A" organ (speak-addr organ) message)))

(defgeneric heard-message (organ from type &rest event)
  (:documentation "A dispatch method for heard messages.")
  (:method ((organ standard-organ) from type &rest event)
    (declare (ignore event))
    (log-for (trace) "~A: Message ~A/~A unhandled." organ from type)))

(defbehavior have-hearing (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message)))
    (apply #'heard-message `(,organ ,@message))))

(defbehavior listen-where-told (:on (:command :listen :from :head) :do :invoke-with-event) (organ event)
  (let ((addr (getf event :listen)))
    (when (and addr (listen-sock organ)
               (not (gethash addr (listening-to organ))))
      (setf (gethash addr (listening-to organ))
            (zmq:connect (listen-sock organ) addr)))))

;; TODO: Maybe something more like (:need (:agent :uuid :from #'parent-uuid) :do invoke) ?
(defbehavior die-without-parent (:interval (:from :heart :nth 6) :do :invoke) (organ)
  (let ((parent (gethash (parent-uuid (organ-agent organ)) (agent-peers organ))))
    (if parent
        (format t "I have a parent: ~A~%" parent)
        (format t "Parent not found.~%"))))

