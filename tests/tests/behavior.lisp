(in-package :afdog-tests)

;; Behavior tests
(def-test (agent-speaks :group basic-behavior-tests :fixtures (running-agent-fixture)) :true
  (let ((msg (make-instance 'zmq:msg)))
    (zmq:with-context (c 1)
      (zmq:with-socket (s c zmq:sub)
        (zmq:setsockopt s zmq:subscribe "")
        (zmq:connect s (agent::local-ipc-addr agent-uuid :mouth))
        (setf msg
              (handler-case (bt:with-timeout (15)
                              (agent::parse-message (agent::read-message s)))
                (bt:timeout () nil)))))
      msg))

(def-test (agent-hears :group basic-behavior-tests :fixtures (running-agent-fixture)) :true
  (let ((msg (make-instance 'zmq:msg :data "(:TEST :PING)"))
        (ponged-p nil))
    (zmq:with-context (c 1)
      (zmq:with-socket (read-sock c zmq:sub)
        (zmq:with-socket (write-sock c zmq:pub)
          (zmq:connect write-sock (agent::local-ipc-addr agent-uuid :ear))
          (zmq:connect read-sock (agent::local-ipc-addr agent-uuid :mouth))
          (zmq:setsockopt read-sock zmq:subscribe "")
          (zmq:send! write-sock msg)
          (handler-case
              (bt:with-timeout (10)
                (do ((msg
                      (agent::parse-message (agent::read-message read-sock))
                      (agent::parse-message (agent::read-message read-sock))))
                     (ponged-p t)
                     (when (equalp (getf msg :test) :pong)
                         (setf ponged-p t))))
                (bt:timeout () ponged-p)))))))

(def-test (agent-sees :group basic-behavior-tests :fixtures (running-agent-fixture)) :true
  (let ((seen-self-p nil))
    (zmq:with-context (c 1)
      (zmq:with-socket (read-sock c zmq:sub)
        (zmq:with-socket (write-sock c zmq:pub)
          (zmq:connect write-sock (agent::local-ipc-addr agent-uuid :ear))
          (zmq:connect read-sock (agent::local-ipc-addr agent-uuid :mouth))
          (zmq:setsockopt read-sock zmq:subscribe "")
          (zmq:send! write-sock (make-instance 'zmq:msg :data "(:look :self)"))
          (handler-case
              (bt:with-timeout (30)
                (do* ((msg
                      (agent::parse-message (agent::read-message read-sock))
                      (agent::parse-message (agent::read-message read-sock))))
                    (seen-self-p t)
                  (when (getf (getf msg :process) :alive)
                      (setf seen-self-p t))))
            (bt:timeout () seen-self-p)))))))

(def-test (agent-watches :group basic-behavior-tests :fixtures (running-agent-fixture))
    (:process (:check
               (:true-form (equalp 0
                                   (let (watching)
                                     (zmq:with-context (ctx 1)
                                       (zmq:with-socket (write-sock ctx zmq:pub)
                                         (zmq:with-socket (read-sock ctx zmq:sub)
                                           (zmq:connect write-sock (agent::local-ipc-addr agent-uuid :ear))
                                           (zmq:connect read-sock (agent::local-ipc-addr agent-uuid :mouth))
                                           (zmq:setsockopt read-sock zmq:subscribe "")

                                           (zmq:send! write-sock (agent::prepare-message '(:count :watching)))
                                           (handler-case (bt:with-timeout (15)
                                                           (do* ((msg (agent::parse-message (agent::read-message read-sock))
                                                                      (agent::parse-message (agent::read-message read-sock))))
                                                                (watching)
                                                             (when (equalp (car msg) :count)
                                                               (setf watching (second msg)))))
                                             (bt:timeout () nil)))))
                                     watching))))

              (:eval (zmq:with-context (ctx 1)
                                       (zmq:with-socket (write-sock ctx zmq:pub)
                                           (zmq:connect write-sock (agent::local-ipc-addr agent-uuid :ear))

                                           (zmq:send! write-sock (agent::prepare-message '(:watch :self))))))

              (:check
               (:true-form (equalp 1
                                   (let (watching)
                                     (zmq:with-context (ctx 1)
                                       (zmq:with-socket (write-sock ctx zmq:pub)
                                         (zmq:with-socket (read-sock ctx zmq:sub)
                                           (zmq:connect write-sock (agent::local-ipc-addr agent-uuid :ear))
                                           (zmq:connect read-sock (agent::local-ipc-addr agent-uuid :mouth))
                                           (zmq:setsockopt read-sock zmq:subscribe "")

                                           (zmq:send! write-sock (agent::prepare-message '(:count :watching)))
                                           (handler-case (bt:with-timeout (15)
                                                           (do* ((msg (agent::parse-message (agent::read-message read-sock))
                                                                      (agent::parse-message (agent::read-message read-sock))))
                                                                (watching)
                                                             (when (equalp (car msg) :count)
                                                               (setf watching (second msg)))))
                                             (bt:timeout () nil)))))
                                     watching))))

              (:check
               (:true-form (let (watching)
                             (zmq:with-context (ctx 1)
                               (zmq:with-socket (write-sock ctx zmq:pub)
                                 (zmq:with-socket (read-sock ctx zmq:sub)
                                   (zmq:connect write-sock (agent::local-ipc-addr agent-uuid :ear))
                                   (zmq:connect read-sock (agent::local-ipc-addr agent-uuid :mouth))
                                   (zmq:setsockopt read-sock zmq:subscribe "")

                                   (zmq:send! write-sock (agent::prepare-message '(:count :watching)))
                                   (handler-case (bt:with-timeout (15)
                                                   (do* ((msg (agent::parse-message (agent::read-message read-sock))
                                                              (agent::parse-message (agent::read-message read-sock))))
                                                        (watching)
                                                     (when (getf (getf msg :process) :alive)
                                                       (setf watching t))))
                                     (bt:timeout () nil)))))
                             watching)))

              (:eval (zmq:with-context (ctx 1)
                                       (zmq:with-socket (write-sock ctx zmq:pub)
                                           (zmq:connect write-sock (agent::local-ipc-addr agent-uuid :ear))

                                           (zmq:send! write-sock (agent::prepare-message '(:stop-watching :self))))))

              (:check
               (:true-form (equalp 0
                                   (let (watching)
                                     (zmq:with-context (ctx 1)
                                       (zmq:with-socket (write-sock ctx zmq:pub)
                                         (zmq:with-socket (read-sock ctx zmq:sub)
                                           (zmq:connect write-sock (agent::local-ipc-addr agent-uuid :ear))
                                           (zmq:connect read-sock (agent::local-ipc-addr agent-uuid :mouth))
                                           (zmq:setsockopt read-sock zmq:subscribe "")

                                           (zmq:send! write-sock (agent::prepare-message '(:count :watching)))
                                           (handler-case (bt:with-timeout (15)
                                                           (do* ((msg (agent::parse-message (agent::read-message read-sock))
                                                                      (agent::parse-message (agent::read-message read-sock))))
                                                                (watching)
                                                             (when (equalp (car msg) :count)
                                                               (setf watching (second msg)))))
                                             (bt:timeout () nil)))))
                                     watching))))))


(def-test (agent-hands-can-make-agents :group basic-behavior-tests :fixtures (spawner-fixture running-hypervisor-fixture)) :true
  (let ((child-uuid (format nil "~A" (uuid:make-v4-uuid))))
    (with-agent-conversation (m e :timeout 10) agent-uuid
      (zmq:send! e (agent::prepare-message `(:make :agent :uuid ,child-uuid)))
      (do ((msg (agent::parse-message (agent::read-message m))
                (agent::parse-message (agent::read-message m))))
          ((equalp (car msg) :made) t)))

    (with-agent-conversation (m e :timeout 30) child-uuid
      (do ((msg (agent::parse-message (agent::read-message m))
                (agent::parse-message (agent::read-message m))))
          ((equalp (subseq msg 0 2) '(:agent :info)) t)))))

(def-test (agent-hands-can-make-processes :group basic-behavior-tests :fixtures (transaction-id-fixture spawner-fixture running-hypervisor-fixture)) :true
  (with-agent-conversation (m e :timeout 20) agent-uuid
    (zmq:send! e (agent::prepare-message `(:make :process :transaction-id ,transaction-id)))
    (do ((msg (agent::parse-message (agent::read-message m))
              (agent::parse-message (agent::read-message m))))
        ((and (equalp (getf msg :made) :process)
              (equal (getf msg :transaction-id) transaction-id)) t))))

(def-test (agent-dies-when-asked :group basic-behavior-tests :fixtures (running-agent-fixture))
    (:seq :true
          (:not :true))
  (list
   ;; Does the agent make a noise when I ask it to die?
   (with-agent-conversation (m e) agent-uuid
     (zmq:send! e (agent::prepare-message `(:agent :kill :kill ,agent-uuid)))
     (do ((msg (agent::parse-message (agent::read-message m))
               (agent::parse-message (agent::read-message m))))
         ;; Expecting to hear: (:agent :death :death ,agent-uuid)
         ((and (>= (length msg) 4)
               (equalp (subseq msg 0 2) '(:agent :death))
               (equalp (getf msg :death) agent-uuid))
          :dead)))

   ;; Does it actually die?
   (with-agent-conversation (m e :timeout 5) agent-uuid
     (do ((msg (agent::parse-message (agent::read-message m))
               (agent::parse-message (agent::read-message m))))
         (nil))
     :did-not-timeout)))
