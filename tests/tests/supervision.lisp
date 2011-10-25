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

(def-eval-test (parent-announces-child-as-peer-correctly :group supervision-tests :fixtures (running-hypervisor-child))
  (let (child-info parent-info)
    (with-agent-conversation (mouth ear :timeout 15) agent-uuid
      (do* ((msg (agent::parse-message (agent::read-message mouth))
                 (agent::parse-message (agent::read-message mouth))))
           ((and (equalp (subseq msg 0 2) '(:AGENT :INFO))
                 (assoc child-uuid (getf (getf msg :info) :peers) :test #'equalp))
            (setf parent-info msg))))

    (with-agent-conversation (mouth ear :timeout 15) child-uuid
      (do* ((msg (agent::parse-message (agent::read-message mouth))
                 (agent::parse-message (agent::read-message mouth))))
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
  nil)

(def-test (child-dies-when-orphaned :group supervision-tests :fixtures (started-parent-and-child)) :true
  (handler-case (bt:with-timeout (20)
                  (loop until (agent::running-p child))
                  (agent::stop parent)
                  (loop while (agent::running-p child))
                  :true)
    (bt:timeout () (format t "Timeout~%") nil)))
