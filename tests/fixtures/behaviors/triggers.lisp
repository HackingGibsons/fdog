(in-package :afdog-tests)

;; Agent test driving behaviors
(agent::defbehavior speak-test-message (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message)))
    (when (equalp message '(:test :ping))
      (agent::send-message organ :command '(:command :speak
                                            :say (:test :pong))))))

(agent::defbehavior look-at-self-when-asked (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message)))
    (when (equalp message '(:look :self))
      (agent::send-message organ :command `(:command :look
                                                     :at (:process :pid :pid ,(iolib.syscalls:getpid)))))))

;; TODO: This only sees procs
(agent::defbehavior announce-what-i-see (:on (:saw :process :from :eye) :do :invoke-with-event) (organ event)
  (log-for (trace agent::organ) "organ: ~A sees pid ~A and alive is ~A" organ (getf event :pid) (getf event :alive))
  (agent::send-message organ :command `(:command :speak
                                       :say ,event)))

(agent::defbehavior watch-self-when-asked (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message)))
    (cond
      ((equalp message '(:watch :self))
       (agent::send-message organ :command `(:command :watch
                                                      :watch (:process :pid :pid ,(iolib.syscalls:getpid)))))

      ((equalp message `(:count :watching))
       (let* ((eye (agent::find-organ (agent::organ-agent organ) :eye))
              (behavior (and eye
                             (find-if #'(lambda (b) (typep b 'agent::watch-when-told)) (agent::behaviors eye))))
              (watching (and behavior
                             (agent::watching behavior))))
         (agent::send-message organ :command `(:command :speak
                                                        :say (:count ,(and watching
                                                                           (hash-table-count watching)))))))

      ((equalp message '(:stop-watching :self))
       (agent::send-message organ :command `(:command :stop-watching
                                                      :stop-watching (:process :pid :pid ,(iolib.syscalls:getpid))))))))
