(in-package :afdog-tests)

(def-test (hyperrunner-is-running :group supervision-tests) :true
  (agent::running-p agent-runner))

(def-test (hypervisor-is-speaking :group supervision-tests) :true
  (with-agent-conversation (mouth-sock ear-sock) agent-uuid
    (agent::parse-message (agent::read-message mouth-sock))))

(def-test (child-runner-exists :group supervision-tests :fixtures (running-hypervisor-child)) :true
  child-runner)

(def-test (child-runner-running :group supervision-tests :fixtures (running-hypervisor-child)) :true
  (agent::running-p child-runner))

(def-test (child-is-speaking :group supervision-tests :fixtures (running-hypervisor-child)) :true
  (with-agent-conversation (mouth-sock ear-sock) child-uuid
    (agent::parse-message (agent::read-message mouth-sock))))

(def-test (parent-notices-child :group supervision-tests :fixtures (running-hypervisor-child)) :true
  (with-agent-conversation (mouth ear :timeout 30) agent-uuid
    (do* ((msg (agent::parse-message (agent::read-message mouth))
               (agent::parse-message (agent::read-message mouth)))
          (peers (getf (getf msg :info) :peers)
                 (getf (getf msg :info) :peers)))
         ((assoc child-uuid peers :test #'equalp) t))))

(def-test (parent-announces-child-as-peer-correctly :group supervision-tests :fixtures (running-hypervisor-child)) :true
  (let (child-info parent-info)
    (with-agent-conversation (mouth ear :timeout 15) agent-uuid
      (do* ((msg (agent::parse-message (agent::read-message mouth))
                 (agent::parse-message (agent::read-message mouth))))
           ((and (equalp (subseq msg 0 2) '(:AGENT :INFO))
                 (getf (getf msg :info) :peers))
            (setf parent-info msg))))

    (with-agent-conversation (mouth ear :timeout 15) child-uuid
      (do* ((msg (agent::parse-message (agent::read-message mouth))
                 (agent::parse-message (agent::read-message mouth))))
           ((equalp (subseq msg 0 2) '(:AGENT :INFO))
            (setf child-info msg))))

    (format t "Parent info so far: ~A~%" parent-info)
    (format t "Child info so far: ~A~%" child-info)
    ;; TODO: Compare what the child announces and what the parents sees and the inverse
    nil))
