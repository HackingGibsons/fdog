(in-package :afdog-tests)

;; Behavior tests
(def-test (agent-speaks :group basic-behavior-tests :fixtures (running-agent-fixture)) :true
  (wait-for-agent (agent-uuid)))

(def-test (agent-hears :group basic-behavior-tests :fixtures (running-agent-fixture)) :true
    (with-agent-conversation (m e) agent-uuid
      (zmq:send! e (prepare-message `(:test :ping)))
      (do ((msg (parse-message (read-message m))
                (parse-message (read-message m))))
          ((equalp (getf msg :test) :pong)
           :ponged))))

(def-test (agent-sees :group basic-behavior-tests :fixtures (running-agent-fixture)) :true
  (with-agent-conversation (m e :timeout 30) agent-uuid
    (zmq:send! e (prepare-message '(:look :self)))
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m))))
         ((getf (getf msg :process) :alive)
          :saw-living-proc))))

(def-test (agent-watches :group basic-behavior-tests :fixtures (running-agent-fixture))
    (:process (:check
               (:true-form (equalp 0
                                   (let (watching)
                                     (with-agent-conversation (m e :timeout 15) agent-uuid
                                       (zmq:send! e (prepare-message '(:count :watching)))
                                       (do* ((msg (parse-message (read-message m))
                                                  (parse-message (read-message m))))
                                            (watching)
                                         (when (equalp (car msg) :count)
                                           (setf watching (second msg)))))
                                     watching))))

              (:eval (with-agent-conversation (m e :linger -1) agent-uuid
                         (zmq:send! e (prepare-message '(:watch :self)))))
              (:check
               (:true-form (equalp 1
                                   (let (watching)
                                     (with-agent-conversation (m e :timeout 15) agent-uuid
                                           (zmq:send! e (prepare-message '(:count :watching)))
                                           (do* ((msg (parse-message (read-message m))
                                                      (parse-message (read-message m))))
                                                ((and watching
                                                      (numberp watching)
                                                      (> watching 0)))
                                             (when (equalp (car msg) :count)
                                               (zmq:send! e (prepare-message '(:count :watching)))
                                               (setf watching (second msg)))))
                                     watching))))
              (:check
               (:true-form (let (watching)
                             (with-agent-conversation (m e :timeout 15) agent-uuid
                                   (zmq:send! e (prepare-message '(:count :watching)))
                                   (do* ((msg (parse-message (read-message m))
                                              (parse-message (read-message m))))
                                        (watching)
                                     (when (getf (getf msg :process) :alive)
                                       (setf watching t))))
                             watching)))

              (:eval (with-agent-conversation (m e :timeout 15 :linger -1) agent-uuid
                         (zmq:send! e (prepare-message '(:stop-watching :self)))))
              (:check
               (:true-form (equalp 0
                                   (let (watching)
                                     (with-agent-conversation (m e :timeout 15) agent-uuid
                                       (zmq:send! e (prepare-message '(:count :watching)))
                                       (do* ((msg (parse-message (read-message m))
                                                  (parse-message (read-message m))))
                                            (watching)
                                         (when (equalp (car msg) :count)
                                           (setf watching (second msg)))))
                                     watching))))))


(def-test (agent-hands-can-make-agents :group basic-behavior-tests :fixtures (spawner-fixture running-hypervisor-fixture))
    (:eql :heard)
  (let ((child-uuid (format nil "~A" (uuid:make-v4-uuid))))
    (and (with-agent-conversation (m e :linger -1) agent-uuid
           ;; Wait for the agent and ask it to make a thing
           (do ((msg (parse-message (read-message m))
                     (parse-message (read-message m))))
               ((getf msg :info)
                (zmq:send! e (prepare-message `(:make :agent :uuid ,child-uuid))))))

         (with-agent-conversation (m e :timeout 30) child-uuid
           (do ((msg (parse-message (read-message m))
                     (parse-message (read-message m))))
               ((getf msg :info)
                :heard))))))

(def-test (agent-hands-can-make-processes :group basic-behavior-tests :fixtures (transaction-id-fixture spawner-fixture running-hypervisor-fixture)) :true
  (with-agent-conversation (m e :timeout 20) agent-uuid
    (zmq:send! e (prepare-message `(:make :process :transaction-id ,transaction-id)))
    (do ((msg (parse-message (read-message m))
              (parse-message (read-message m))))
        ((and (equalp (getf msg :made) :process)
              (equal (getf (getf msg :process) :transaction-id) transaction-id)) t))))

(def-test (agent-dies-when-asked :group basic-behavior-tests :fixtures (running-agent-fixture))
    (:seq (:eql :running)
          (:eql :death)
          (:eql :not-running))
  (list
   (and (running-p agent-runner)
        :running)

   ;; Let's hear a death message
   (with-agent-conversation (m e) agent-uuid
     (zmq:send! e (prepare-message `(:agent :kill :kill ,agent-uuid)))
     (do ((msg (parse-message (read-message m :timeout 1))
               (parse-message (read-message m :timeout 1))))
         ((or (getf msg :death)
              (not (running-p agent-runner))) ;; Maybe we missed it that quickly
          :death)))

   ;; Does it actually die?
   (with-agent-conversation (m e) agent-uuid
     ;; Tell it to, just in case
     (zmq:send! e (prepare-message `(:agent :kill :kill ,agent-uuid)))
     (do ((alive (running-p agent-runner) (running-p agent-runner)))
         ((not alive)
          :not-running)))))

(def-test (agent-notices-when-peers-die-in-silence :group basic-behavior-tests :fixtures (started-parent-and-child))
    (:seq (:eql :found) ;; Find a peer
          (:eql :dead)  ;; Kill the peer
          (:eql :gone)) ;; See that the parent notices
  (list
    ;; Wait for a parent to acknoledge the peer
    (with-agent-conversation (m e :timeout 20) uuid
      (do* ((msg (parse-message (read-message m))
                 (parse-message (read-message m)))
            (info nil (getf msg :info)))
          ((and (equalp (subseq msg 0 2) '(:agent :info))
                info
                (getf info :peers)) :found)))

    ;; Kill the child and make sure it dies
    (with-agent-conversation (m e) kid-uuid
     (zmq:send! e (prepare-message `(:agent :kill :kill ,kid-uuid)))
     (do ((msg (parse-message (read-message m))
               (parse-message (read-message m))))
         ;; Expecting to hear: (:agent :death :death ,kid-uuid)
         ((and (>= (length msg) 4)
               (equalp (subseq msg 0 2) '(:agent :death))
               (equalp (getf msg :death) kid-uuid))
          :dead)))

    ;; Try to find an agent info with a nil peers list
    (with-agent-conversation (m e :timeout 30) uuid
      (do* ((msg (parse-message (read-message m :timeout 1))
                 (parse-message (read-message m :timeout 1)))
            (info (getf msg :info) (getf msg :info)))
          ((and info
                (not (find kid-uuid (getf info :peers) :key #'car :test #'equalp)))
           :gone)))))

(def-test (agent-dies-after-timeout :group basic-behavior-tests :fixtures (running-agent-fixture)) :true
  ;; Send a message to agent's head to change the interval to 10 seconds
  ;; after a 15 second timeout, agent should be dead
  (with-agent-conversation (m e :timeout 30) agent-uuid
    (zmq:send! e (prepare-message `(:new-timeout-interval 10)))
    (do (running (running-p agent-runner (running-p agent-runner)))
        ((not running) :died))))

(def-test (agent-can-look-at-directory-contents :group basic-behavior-tests :fixtures (running-agent-fixture)) :true
  (with-agent-conversation (m e :timeout 20) agent-uuid
    (zmq:send! e (prepare-message `(:look :directory :path ,(namestring (asdf:system-source-directory :afdog)))))
    (do ((msg (parse-message (read-message m))
              (parse-message (read-message m))))
        ((and (equalp (getf msg :saw) :directory)
              (getf msg :exists)
              (string-equal (pathname-type (find "afdog"
                                                 (mapcar #'pathname (getf msg :contents))
                                                 :key #'pathname-name
                                                 :test #'string-equal))
                            "asd"))
         t))))


(def-test (agent-can-look-at-empty-directory :group basic-behavior-tests :fixtures (running-agent-fixture empty-directory-fixture)) :true
  (with-agent-conversation (m e :timeout 20) agent-uuid
    (zmq:send! e (prepare-message `(:look :directory :path ,(namestring empty-directory))))
    (do ((msg (parse-message (read-message m))
              (parse-message (read-message m))))
        ((and (equalp (getf msg :saw) :directory)
              (getf msg :exists)
              (null (getf msg :contents)))
         t))))

(def-test (agent-can-fail-to-find-directory :group basic-behavior-tests :fixtures (running-agent-fixture)) :true
  (with-agent-conversation (m e :timeout 20) agent-uuid
    (zmq:send! e (prepare-message `(:look :directory :path ,(format nil "~A" (uuid:make-v4-uuid)))))
    (do ((msg (parse-message (read-message m))
              (parse-message (read-message m))))
        ((and (equalp (getf msg :saw) :directory)
              (not (getf msg :exists))
              (null (getf msg :contents)))
         t))))

(def-test (agent-opens-common-mouth :group basic-behavior-tests :fixtures (running-agent-fixture))
    :true
  (discover-agents-on-host () (uuid info)
    (declare (ignorable info))
    uuid))

(def-test (can-discover-parent-and-child :group basic-behavior-tests :fixtures (started-parent-and-child))
    :true
    (flet ((has-both-uuids (found-uuids)
             (and (find uuid found-uuids :test #'equalp)
                  (find kid-uuid found-uuids :test #'equalp))))
      (let (found)
        (discover-agents-on-host (:traverse t) (uuid info)
          (declare (ignorable info))
          (has-both-uuids
           (pushnew uuid found :test #'equalp))))))

(def-test (duplicate-agents-kill-themselves :group basic-behavior-tests :fixtures (running-agent-fixture)) (:eql :agent-dies)
  (progn
  ;; Forge an agent info message from an older agent
  (with-agent-conversation (m e :timeout 30) agent-uuid
    (zmq:send! e (prepare-message `(:agent :info :info (:uuid ,agent-uuid :timestamp ,(get-universal-time) :age 999999))))

    ;; Younger agent should die
    (do ((running (running-p agent-runner)
                  (running-p agent-runner)))
        ((not running)
         :agent-dies)))))

(def-test (agent-lives-if-younger-agent-announces :group basic-behavior-tests :fixtures (running-agent-fixture)) (:eql :agent-lives)
  (progn
    ;; Forge an agent info mesage from a younger agent
    (with-agent-conversation (m e :timeout 20) agent-uuid
      (zmq:send! e (prepare-message `(:agent :info :info (:uuid ,agent-uuid :timestamp ,(get-universal-time) :age -999999))))

      ;; The older agent should live. We can test this by looking for agent info messages
      (do ((msg (parse-message (read-message m))
                (parse-message (read-message m))))
        ((and (equalp (getf msg :agent) :info)
              (equalp (getf (getf msg :info) :uuid) agent-uuid))
         (and (running-p agent-runner) :agent-lives))))))

(def-test (agent-writes-crash-report :group basic-behavior-tests :fixtures (running-agent-fixture))
    (:values (:eql :running)
             (:eql :seems-to-crash)
             (:eql :report-written)
             (:eql :report-cleaned))
  (wait-for-agent (agent-uuid) :running)

  (and (with-agent-conversation (m e :linger -1) agent-uuid
         (zmq:send! m (prepare-message `(:agent :crash :crash :agent :agent ,agent-uuid))))
       (not (wait-for-agent (agent-uuid :timeout 3) :still-running))
       :seems-to-crash)

  (and (find-if (arnesi:curry #'ppcre:scan (format nil "crash.+~A\." agent-uuid))
                (mapcar #'namestring (directory (make-pathname :directory `(:absolute "tmp")
                                                               :type "log" :name :wild))))
       :report-written)

  (and (mapcar #'delete-file
               (mapcar #'namestring (directory (make-pathname :directory `(:absolute "tmp")
                                                               :type "log" :name :wild))))
       :report-cleaned))
