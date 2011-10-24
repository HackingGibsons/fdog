(in-package :afdog-tests)

;; Agents
(defclass test-agent (standard-agent)
  ()
  (:documentation "A `standard-agent' derivative we can insert probes into to test things."))

(defclass runner-agent (standard-agent)
  ()
  (:documentation "A `standard-agent' derivative we can insert probes into to test things."))

(defmethod agent-special-event :after ((agent runner-agent) (head (eql :boot)) event)
  (make-speak-test-message (agent::find-organ agent :head))

  ;; Blatant test drivers
  (make-look-at-self-when-asked (agent::find-organ agent :head))
  (make-watch-self-when-asked (agent::find-organ agent :head))

  (make-announce-what-i-see (agent::find-organ agent :head)))


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


;;
;; Runner encapsulation
(defclass test-runner (agent::exec-runner) ())

(defmethod update-instance-for-different-class :after (previous (current test-runner) &key)
  (setf (agent::init-forms current)
        (append (agent::init-forms current)
                '((ql:quickload :afdog-tests)))))

(defmethod agent::make-runner ((style (eql :test)) &rest keys &key)
  (change-class (apply #'agent::make-runner :exec keys) 'test-runner))

;; Test fixtures
(def-fixtures agent-fixture
    (:special (agent))
  (agent (make-instance 'test-agent)))

(def-fixtures running-agent-fixture
    (:setup
     (unless (agent::running-p agent-runner)
       (agent::start agent-runner))
     :cleanup
     (if (agent::running-p agent-runner)
         (agent::stop agent-runner)))
  (agent-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (agent-runner (agent::make-runner :test :class 'afdog-tests::runner-agent :include '(:afdog-tests) :uuid agent-uuid)))
