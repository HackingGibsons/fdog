(in-package :afdog-tests)

;; Agent test driving behaviors
(defbehavior speak-test-message (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message)))
    (when (equalp message '(:test :ping))
      (send-message organ :command '(:command :speak
                                     :say (:test :pong))))))

(defbehavior look-at-self-when-asked (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message)))
    (when (equalp message '(:look :self))
      (send-message organ :command `(:command :look
                                              :at (:process :pid :pid ,(iolib.syscalls:getpid)))))))

(defbehavior look-at-child-when-asked (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message)))
    (when (equalp (getf message :look) :child)
      (send-message organ :command `(:command :look
                                              :at (:agent :uuid :uuid ,(getf message :uuid)))))))

(defbehavior look-at-directory-when-asked (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message)))
    (when (equalp (getf message :look) :directory)
      (send-message organ :command `(:command :look
                                              :at (:directory :path :path ,(getf message :path)))))))

(defbehavior announce-what-i-see (:or ((:on (:saw :process :from :eye))
                                       (:on (:saw :agent :from :eye))
                                       (:on (:saw :directory :from :eye)))
                                      :do :invoke-with-event) (organ event)
  (log-for (trace organ) "organ: ~A sees pid ~A and alive is ~A" organ (getf event :pid) (getf event :alive))
  (send-message organ :command `(:command :speak
                                          :say ,event)))

(defbehavior announce-what-i-make (:or ((:on (:made :agent :from :hand))
                                        (:on (:made :process :from :hand)))
                                       :do :invoke-with-event) (organ event)
  (send-message organ :command `(:command :speak
                                          :say ,event)))

(defbehavior announce-when-links-collide (:on (:already-linked :process :from :head) :do :invoke-with-event) (organ event)
  (send-message organ :command `(:command :speak
                                          :say ,event)))

(defbehavior announce-what-i-unlink (:or ((:on (:unlinked :agent :from :head))
                                          (:on (:unlinked :process :from :head)))
                                         :do :invoke-with-event) (organ event)
  (send-message organ :command `(:command :speak :say ,event)))

(defbehavior spawn-dependant-when-asked (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message))
        (uuid (format nil "~A" (uuid:make-v4-uuid))))
    (when (equalp message '(:spawn :child))
      (send-message organ :command `(:command :speak
                                                     :say (:echo :spawn :child ,uuid)))
      (send-message organ :command `(:command :link
                                                     :link :agent
                                                     :agent (:uuid ,uuid :class leaf-test-agent :package :afdog-tests))))))

(defbehavior link-to-process-when-asked (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message))
        (uuid (prin1-to-string (uuid:make-v4-uuid))))
    (when (and (eql (car message) :link-running)
               (getf message :pid))
      (send-message organ :command `(:command :link
                                     :link :process
                                     :process (:pid ,(getf message :pid) :path ,(format nil "~A/sleep.sh" *root*) :args (,(prin1-to-string (uuid:make-v4-uuid)))))))))

(defbehavior spawn-process-when-asked (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message))
        (uuid (format nil "~A" (uuid:make-v4-uuid))))
    (when (equalp message '(:spawn :process))
      (send-message organ :command `(:command :link
                                     :link :process
                                     :process (:pid nil
                                               :path ,(format nil "~A/sleep.sh" *root*) :args ("no")))))))

(defbehavior unlink-when-asked (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message)))
    (when (find (getf message :unlink) '(:agent :process))
      (send-message organ :command `(:command :unlink ,@message)))))

(defbehavior watch-self-when-asked (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message)))
    (cond
      ((equalp message '(:watch :self))
       (send-message organ :command `(:command :watch
                                               :watch (:process :pid :pid ,(iolib.syscalls:getpid)))))

      ((equalp message `(:count :watching))
       (let* ((eye (find-organ (organ-agent organ) :eye))
              (behavior (and eye
                             (find-if #'(lambda (b) (typep b 'watch-when-told)) (behaviors eye))))
              (watching (and behavior
                             (watching behavior))))
         (send-message organ :command `(:command :speak
                                                 :say (:count ,(and watching
                                                                    (hash-table-count watching)))))))

      ((equalp message '(:stop-watching :self))
       (send-message organ :command `(:command :stop-watching
                                               :stop-watching (:process :pid :pid ,(iolib.syscalls:getpid))))))))

(defbehavior make-agent-when-asked (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let* ((message (getf event :message)))
    (when (and (> (length message) 3)
               (equalp (subseq message 0 3) '(:make :agent :uuid)))
      (send-message organ :command `(:command :make
                                                     :make :agent
                                                     :agent (:uuid ,(getf message :uuid)
                                                                   :class leaf-test-agent
                                                                   :package :afdog-tests))))))

(defbehavior make-process-when-asked (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let* ((message (getf event :message)))
    (when (and (> (length message) 3)
               (equalp (subseq message 0 2)  '(:make :process)))

      (send-message organ :command `(:command :make
                                              :make :process
                                              :process (:path "/usr/bin/env"
                                                              :args ("echo")
                                                              :transaction-id ,(getf message :transaction-id)))))))

(defclass timeout-mixin ()
  ((start-time :initform (get-internal-real-time)
               :accessor start-time
               :documentation "Time the agent was started")
   (timeout :initform (* 120 internal-time-units-per-second)
            :accessor timeout
            :documentation "Interval in seconds before an agent kills itself. Defaults to 2 minutes")))

(defbehavior kill-self-after-timeout (:or ((:on (:heard :message :from :ear))
                                                  (:interval (:from :heart :nth 1)))
                                             :include (timeout-mixin)
                                             :do :invoke-with-event) (organ event)
  (log-for (trace) "event: ~A" event)
  (let ((new-timeout-interval (getf (getf event :message) :new-timeout-interval))
        (reset-timeout (eql (getf (getf event :message) :reset) :timeout)))
    (log-for (trace) "new-timeout-interval: ~A" new-timeout-interval)
    (log-for (trace) "reset-timeout: ~A" reset-timeout)
    (cond
      (new-timeout-interval
        (log-for (trace) "new timeout interval: ~A" new-timeout-interval)
        (setf (timeout behavior) (* new-timeout-interval internal-time-units-per-second)))

      (reset-timeout
        (log-for (trace) "resetting timeout")
        (setf (start-time behavior) (get-internal-real-time)))

      (t
        (log-for (trace) "no new interval, current time: ~A, timeout interval ~A" (- (get-internal-real-time) (start-time behavior)) (timeout behavior))  )))
  (when (>= (- (get-internal-real-time) (start-time behavior)) (timeout behavior))
    (log-for (warn) "~A: Timeout reached, killing myself" organ)
    (suicide (organ-agent organ))))

(defbehavior forge-agent-info (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message)))
    (when (equalp (getf message :agent) :info)
      (agent::agent-send-message (organ-agent organ) message))))
