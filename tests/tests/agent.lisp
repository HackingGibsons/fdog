(in-package :afdog-tests)

;; Test cases
(def-test (can-test-nothing :group basic-tests) :true
  t)

(def-test (fresh-agent-is-fresh :group basic-tests :fixtures (agent-fixture))
    (:all (:apply agent-event-count (:predicate zerop))
          (:apply agent-context (:not :true)))
  agent)

;; Drivers for the internal tests
(defmethod agent-special-event :after ((agent test-agent) (head (eql :boot)) event)
  "Boot event for the test agent, runs the `booted-agent-tests' group"
  (format t "Agent is booting.~%")
  (nst:nst-cmd :run-group booted-agent-tests))

(defmethod next-event :after ((agent test-agent))
  (case (agent::agent-event-count agent)
    ;; Run the event tests
    (5 (nst:nst-cmd :run-group running-with-events-tests))

    ;; Remove an organ to test death from system failure
    ;; should race a timeout to fail
    (6 (let ((organ (agent::find-organ agent :appendix)))
         (zmq:close (agent::organ-incoming-sock organ))
         (zmq:close (agent::organ-outgoing-sock organ))
         (setf (agent::organ-incoming-sock organ) nil
               (agent::organ-outgoing-sock organ) nil)
         (setf (agent::agent-organs agent)
               (remove :appendix (agent::agent-organs agent) :key #'agent::organ-tag))))))

;; This test will scaffold a running agent and run any tests driven by the event loop
;; then execute the terminated agent group
(def-test (test-running-agent :group basic-tests :fixtures (agent-fixture))
    (:process (:eval (handler-case (bt:with-timeout (30)
                                     (run-agent agent))
                       (bt:timeout ()
                         (format t "Timing out!~%")
                         nil)))
              (:eval (nst:nst-cmd :run-group terminated-agent-tests))))

;; Booted agent tests
(def-test (running-agent-sanity-check :group booted-agent-tests) :true
  agent)

(def-test (running-agent-has-context :group booted-agent-tests) :true
  (agent-context agent))

(def-test (organs-have-sockets :group booted-agent-tests)
    (:each (:all (:apply agent::organ-incoming-sock :true)
                 (:apply agent::organ-outgoing-sock :true)))
  (agent-organs agent))

;; Termianted agent tests
(def-test (organs-closed-sockets :group terminated-agent-tests)
    (:each (:all (:apply agent::organ-incoming-sock (:not :true))
                 (:apply agent::organ-outgoing-sock (:not :true))))
  (agent-organs agent))

;; Tests to run after some events fire
(def-test (running-agent-has-events :group running-with-events-tests)
    (:predicate (lambda (v) (> v 0)))
  (agent-event-count agent))


;; Test spawning an agent
(def-test (agent-starts :group runner-tests) :true
  (agent::running-p agent-runner))

(def-test (agent-stops :group runner-tests)
    (:process (:eval (agent::stop agent-runner))
              (:check (:not (:true-form (agent::running-p agent-runner))))))

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
