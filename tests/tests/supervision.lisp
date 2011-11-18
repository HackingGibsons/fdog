(in-package :afdog-tests)

(def-test (hyperrunner-is-running :group supervision-tests) :true
  (running-p agent-runner))

(def-test (hypervisor-is-speaking :group supervision-tests) :true
  (with-agent-conversation (mouth-sock ear-sock) agent-uuid
    (parse-message (read-message mouth-sock))))

(def-test (child-runner-exists :group supervision-tests :fixtures (running-hypervisor-child)) :true
  child-runner)

(def-test (child-runner-running :group supervision-tests :fixtures (running-hypervisor-child)) :true
  (running-p child-runner))

(def-test (child-is-speaking :group supervision-tests :fixtures (running-hypervisor-child)) :true
  (with-agent-conversation (mouth-sock ear-sock) child-uuid
    (parse-message (read-message mouth-sock))))

(def-test (parent-notices-child :group supervision-tests :fixtures (running-hypervisor-child)) :true
  (with-agent-conversation (mouth ear :timeout 30) agent-uuid
    (do* ((msg (parse-message (read-message mouth))
               (parse-message (read-message mouth)))
          (peers (getf (getf msg :info) :peers)
                 (getf (getf msg :info) :peers)))
         ((assoc child-uuid peers :test #'equalp) t))))

(def-eval-test (parent-announces-child-as-peer-correctly :group supervision-tests :fixtures (running-hypervisor-child))
    (let (child-info parent-info)
      (with-agent-conversation (mouth ear) agent-uuid
        (do* ((msg (parse-message (read-message mouth))
                   (parse-message (read-message mouth))))
             ((and (equalp (subseq msg 0 2) '(:AGENT :INFO))
                   (assoc child-uuid (getf (getf msg :info) :peers) :test #'equalp))
              (setf parent-info msg))))

      (with-agent-conversation (mouth ear) child-uuid
        (do* ((msg (parse-message (read-message mouth))
                   (parse-message (read-message mouth))))
             ((and (equalp (subseq msg 0 2) '(:AGENT :INFO))
                   (getf (getf msg :info) :peers))
              (setf child-info msg))))

      (assert-non-nil child-info :format "Didn't get `child-info'")
      (assert-non-nil parent-info :format "Didn't get `parent-info'")

      (assert-non-nil (getf child-info :info) :format "Didn't get `child-info' with :info")
      (assert-non-nil (getf parent-info :info) :format "Didn't get `parent-info' with :info")

      (assert-equalp (getf (getf child-info :info) :parent)
                     (getf (getf parent-info :info) :uuid)
                     :format ":parent on child and :uuid on parent don't match.")

      (assert-non-nil (getf (getf child-info :info) :peers)
                      :format "Child should see some peers.")
      (assert-non-nil (getf (getf parent-info :info) :peers)
                      :format "Parent should see some peers.")

      (let* ((child-ear (getf (getf child-info :info) :ear))
             (child-mouth (getf (getf child-info :info) :mouth))
             (parent-peers (getf (getf parent-info :info) :peers))
             (parent-child-peer (assoc child-uuid (getf (getf parent-info :info) :peers) :test #'equalp)))

        (assert-non-nil child-ear :format "Child should have mentioned an ear.")
        (assert-non-nil child-mouth :format "Child should have mentioned a mouth.")
        (assert-non-nil parent-peers :format "Parent should have peers.")

        (assert-non-nil parent-child-peer :format "Parent should know about the child by UUID"))))

(def-test (parent-can-use-eyes-to-see-child :group supervision-tests :fixtures (running-hypervisor-child)) :true
  (with-agent-conversation (m e :timeout 30) agent-uuid
    (let ((message `(:look :child :uuid ,child-uuid)))
      (do* ((msg (parse-message (read-message m))
                 (parse-message (read-message m))))
           ((and (getf msg :saw) (equalp (getf msg :saw) :agent)
                 (and (not (null (getf (getf msg :agent) :info)))
                      (equalp (getf (getf msg :agent) :uuid) child-uuid))) t)
        (zmq:send! e (prepare-message message))))))

(def-test (agents-cant-see-ghosts :group supervision-tests) :true
  (let ((uuid (format nil "~A" (uuid:make-v4-uuid))))
    (with-agent-conversation (m e :timeout 30) agent-uuid
      (let ((message `(:look :child :uuid ,uuid)))
        (do* ((msg (parse-message (read-message m))
                   (parse-message (read-message m))))
             ((and (getf msg :saw) (equalp (getf msg :saw) :agent)
                   (and (equalp (getf (getf msg :agent) :uuid) uuid)
                        (equalp (getf (getf msg :agent) :info) nil))) t)
          (zmq:send! e (prepare-message message)))))))

(def-test (child-dies-when-orphaned :group supervision-tests :fixtures (started-parent-and-child)) :true
  (handler-case (bt:with-timeout (30)
                  (loop until (running-p child))
                  (stop parent)
                  (loop while (running-p child))
                  :true)
    (bt:timeout () nil)))

(def-test (agent-can-spawn-child :group supervision-tests) :true
  (let (child-uuid)
    (with-agent-conversation (m e :timeout 20) agent-uuid
      (zmq:send! e (agent::prepare-message `(:spawn :child)))
      (do* ((msg (agent::parse-message (agent::read-message m))
                 (agent::parse-message (agent::read-message m))))
           ((equalp (subseq msg 0 2) '(:echo :spawn))
            (setf child-uuid (getf msg :child)))))

    (when child-uuid
      (with-agent-conversation (cm ce :timeout 20) child-uuid
        (do* ((msg (agent::parse-message (agent::read-message cm))
                   (agent::parse-message (agent::read-message cm))))
             ((equalp (subseq msg 0 2) '(:AGENT :INFO))
              t))))))

(def-test (agent-watches-spawned-child :group supervision-tests) :true
  (let (child-uuid)
    (with-agent-conversation (m e :timeout 20) agent-uuid
      (zmq:send! e (agent::prepare-message `(:spawn :child)))
      (do* ((msg (agent::parse-message (agent::read-message m))
                 (agent::parse-message (agent::read-message m))))
           ((equalp (subseq msg 0 2) '(:echo :spawn))
            (setf child-uuid (getf msg :child)))))

    (when child-uuid
      (with-agent-conversation (cm ce :timeout 20) agent-uuid
        (do* ((msg (agent::parse-message (agent::read-message cm))
                   (agent::parse-message (agent::read-message cm))))
             ((and (equalp (second msg) :saw)
                   (equalp (getf msg :saw) :agent)
                   (equalp (getf (getf msg :agent) :uuid) child-uuid))
              t))))))

(def-test (agent-restarts-killed-child :group supervision-tests) :true
  (let (child-uuid)
    (with-agent-conversation (m e :timeout 20) agent-uuid
      (zmq:send! e (agent::prepare-message `(:spawn :child)))
      (do* ((msg (agent::parse-message (agent::read-message m))
                 (agent::parse-message (agent::read-message m))))
           ((equalp (subseq msg 0 2) '(:echo :spawn))
            (setf child-uuid (getf msg :child)))))

    (when child-uuid
      (with-agent-conversation (cm ce :timeout 20) agent-uuid
        (do* ((msg (agent::parse-message (agent::read-message cm))
                   (agent::parse-message (agent::read-message cm))))
             ((and (equalp (second msg) :saw)
                   (equalp (getf msg :saw) :agent)
                   (equalp (getf (getf msg :agent) :uuid) child-uuid))
              t))))

    (with-agent-conversation (m e) child-uuid
      (zmq:send! e (prepare-message `(:agent :kill :kill ,child-uuid)))
      (do ((msg (parse-message (read-message m))
                (parse-message (read-message m))))
          ;; Expecting to hear: (:agent :death :death ,child-uuid)
          ((and (>= (length msg) 4)
                (equalp (subseq msg 0 2) '(:agent :death))
                (equalp (getf msg :death) child-uuid))
           :dead)))

    (when child-uuid
      (with-agent-conversation (cm ce :timeout 20) agent-uuid
        (do* ((msg (agent::parse-message (agent::read-message cm))
                   (agent::parse-message (agent::read-message cm))))
             ((and (equalp (second msg) :saw)
                   (equalp (getf msg :saw) :agent)
                   (equalp (getf (getf msg :agent) :uuid) child-uuid))
              t))))))

(def-test (agent-can-link-to-running-process :group supervision-tests :fixtures (running-process-fixture)) :true
  (with-agent-conversation (m e) agent-uuid
    (zmq:send! e (prepare-message `(:link-running :proccess :pid ,process-pid)))
    (do ((msg (parse-message (read-message m))
              (parse-message (read-message m))))
        ((and (getf msg :eye) (getf msg :saw) (getf msg :process)
              (getf (getf msg :process) :pid)
              (equalp (getf (getf msg :process) :pid) process-pid))
         (getf (getf msg :process) :pid)))))

(def-test (agent-starts-linked-process :group supervision-tests :fixtures (pid-fixture)) :true
  (with-agent-conversation (m e :timeout 60) agent-uuid
    (zmq:send! e (prepare-message `(:spawn :process)))
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (made (getf msg :made) (getf msg :made))
          (made-info (getf msg made)
                     (or made-info (getf msg made))))
      ((and (equalp made :process)
            (getf made-info :pid))
       (setf pid (getf made-info :pid)))
      pid)))

(def-test (agent-restarts-killed-process :group supervision-tests :fixtures (pid-fixture))
    (:seq :true
          (:predicate numberp)
          :true
          :true
          :true
          :true)

  (list
   (with-agent-conversation (m e :timeout 30) agent-uuid
     (do ((msg (parse-message (read-message m))
               (parse-message (read-message m))))
         ((equalp (subseq msg 0 2) '(:agent :info))
          t)))

   (with-agent-conversation (m e :timeout 35) agent-uuid
     (zmq:send! e (prepare-message `(:spawn :process)))
     (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (made (getf msg :made) (getf msg :made))
          (made-info (getf msg made)
                     (or made-info (getf msg made))))
      ((and (equalp made :process)
            (getf made-info :pid))
       (setf pid (getf made-info :pid)))))

   (when pid
     (with-agent-conversation (m e :timeout 35) agent-uuid
       (do ((msg (parse-message (read-message m))
                 (parse-message (read-message m))))
           ((and (getf msg :saw)
                 (equalp (getf msg :saw) :process)
                 (getf (getf msg :process) :pid) pid)
            pid)))
     ;; kill -9 the process to simulate it dying
     (ignore-errors (iolib.syscalls:kill pid iolib.syscalls:sigkill)))

   ;; Get the new pid
   (with-agent-conversation (m e :timeout 35) agent-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (made (getf msg :made) (getf msg :made))
           (made-info (getf msg made)
                      (or made-info (getf msg made))))
          ((and (equalp made :process)
                (getf made-info :pid))
           (setf old-pid pid)
           (setf pid (getf made-info :pid)))))

   (when pid
     (with-agent-conversation (m e :timeout 35) agent-uuid
       ;; Make sure we've seen it
       (do ((msg (parse-message (read-message m))
                 (parse-message (read-message m))))
           ((and (getf msg :saw)
                 (equalp (getf msg :saw) :process)
                 (getf (getf msg :process) :pid)
                 pid)
            pid))))

   (not (equalp pid old-pid))))

(def-test (agent-can-unlink-agents :group supervision-tests) :true
  ;; Start an agent
  (let (child-uuid)
    (with-agent-conversation (m e :timeout 20) agent-uuid
      (zmq:send! e (prepare-message `(:spawn :child)))
      (do* ((msg (parse-message (read-message m))
                 (parse-message (read-message m))))
           ((equalp (subseq msg 0 2) '(:echo :spawn))
            (setf child-uuid (getf msg :child)))))

    (when child-uuid
      (with-agent-conversation (m e :timeout 20) agent-uuid
        ;; Unlink it
        (zmq:send! e (prepare-message `(:unlink :agent :uuid ,child-uuid)))
        ;; Listen for a callback
        (do* ((msg (parse-message (read-message m))
                   (parse-message (read-message m))))
             ((and (eql (getf msg :unlinked) :agent) (equalp (getf msg :uuid) child-uuid))
              t))))))

(def-test (agent-can-unlink-process :group supervision-tests :fixtures (pid-fixture)) :true
   (with-agent-conversation (m e :timeout 20) agent-uuid
    (zmq:send! e (prepare-message `(:spawn :process)))
     (do ((msg (parse-message (read-message m))
               (parse-message (read-message m))))
      ((equalp (getf msg :made) :process)
       (setf pid (getf msg :pid)))))

  (with-agent-conversation (m e :timeout 20) agent-uuid
    (zmq:send! e (prepare-message `(:unlink :process :pid ,pid)))
    (do ((msg (parse-message (read-message m))
              (parse-message (read-message m))))
        ((and (eql (getf msg :unlinked) :process) (equalp (getf msg :pid) pid))
         t))))
