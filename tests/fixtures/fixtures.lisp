(in-package :afdog-tests)

;; Runner encapsulation
(defclass test-runner (exec-runner) ())

(defmethod update-instance-for-different-class :after (previous (current test-runner) &key)
  (setf (init-forms current)
        (append (init-forms current)
                '((ql:quickload :afdog-tests)))))

(defmethod make-runner ((style (eql :test)) &rest keys &key)
  (change-class (apply #'make-runner :exec keys) 'test-runner))

;; Test fixtures
(def-fixtures agent-fixture
    (:special (agent))
  (agent (make-instance 'test-agent)))

(def-fixtures running-hypervisor-fixture
    (:setup
     (start agent-runner)

     :cleanup
     (stop agent-runner))

  (agent-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (agent-runner (make-runner :test :include '(:afdog-tests)
                             :class 'hypervisor-test-agent
                             :uuid agent-uuid)))

(def-fixtures running-hypervisor-child
    (:setup (start child-runner)
            :cleanup (stop child-runner))

  (child-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (child-runner (make-runner :test :include '(:afdog-tests)
                             :class 'leaf-test-agent
                             :uuid child-uuid
                             :parent-uuid agent-uuid
                             :parent-mouth (local-ipc-addr agent-uuid :mouth))))

(def-fixtures started-parent-and-child
    (:setup (progn (start parent)
                   (start child))

            :cleanup (progn (stop parent)
                            (stop child)))

  (uuid (format nil "~A" (uuid:make-v4-uuid)))
  (kid-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (parent (make-runner :test :include '(:afdog-tests)
                       :class 'hypervisor-test-agent
                       :uuid uuid))
  (child (make-runner :test :include '(:afdog-tests)
                      :class 'leaf-test-agent
                      :uuid kid-uuid
                      :parent-uuid uuid
                      :parent-mouth (local-ipc-addr uuid :mouth))))

(def-fixtures running-agent-fixture
    (:setup
     (unless (running-p agent-runner)
       (start agent-runner))

     :cleanup
     (if (running-p agent-runner)
         (stop agent-runner)))

  (agent-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (agent-runner (make-runner :test :include '(:afdog-tests)
                             :class 'afdog-tests::runner-agent
                             :uuid agent-uuid)))

(def-fixtures spawner-fixture (:special (*spawner*))
  (*spawner* :test))

(def-fixtures transaction-id-fixture ()
  (transaction-id (format nil "~A" (uuid:make-v4-uuid))))

(def-fixtures empty-directory-fixture
    (:setup
     (ensure-directories-exist empty-directory)
     :cleanup
     (sb-posix:rmdir empty-directory))
  (empty-directory (asdf:system-relative-pathname :afdog
                                                  (format nil "~A/" (uuid:make-v4-uuid))
                                                  :type :directory)))

(def-fixtures two-pid-fixture
  (:cleanup (progn
              (ignore-errors (iolib.syscalls:kill pid iolib.syscalls:sigkill))
              (ignore-errors (iolib.syscalls:kill old-pid iolib.syscalls:sigkill)))
   :documentation "A fixture to hold two pids for comparison and kill them at the end of test.")
  (old-pid)
  (pid))

(def-fixtures running-process-fixture
 (:cleanup (ignore-errors (iolib.syscalls:kill process-pid iolib.syscalls:sigkill))

  :documentation "A fixture that holds a running process.
Does kill -9 to ensure the process dies in cleanup.")

  (process (afdog:run-program (format nil "~A/sleep.sh" *root*) `(,(prin1-to-string (uuid:make-v4-uuid))) :wait nil))
  (process-pid (sb-ext:process-pid process)))

(defclass test-state-machine (standard-state-machine)
  ((booted :initform nil :accessor test-machine-booted :documentation "When the boot event is fired this should be set to true."))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "A test state machine to assert against."))

(defmethod standard-state-machine-event :after ((machine test-state-machine) (state (eql :initial)) info)
  (setf (test-machine-booted machine) t))

(defstate test-state-machine :initial (info)
  (when (eql info '(:event :test))
    :test))

(defstate test-state-machine :test (info)
  :test)

(def-fixtures test-state-machine-fixture
    (:documentation "A fixture that instantiates a test state machine.")
  (test-machine (make-instance 'test-state-machine)))

(def-fixtures db-path-fixture
    (:documentation "Provides path to an sqlite database."
     :setup (progn
              (ignore-errors (clsql:disconnect) (fdog-models:disconnect))
              (ignore-errors (sb-ext:delete-directory db-root :recursive t)))
     :cleanup (progn
                (ignore-errors (clsql:disconnect) (fdog-models:disconnect))
                (ignore-errors (sb-ext:delete-directory db-root :recursive t))))
  (db-root (merge-pathnames (make-pathname :directory fdog-models:*server-dir*)
                            *root*))
  (db-path (merge-pathnames fdog-models:*config-file*
                            db-root))
  (connected-p nil))

(def-fixtures mongrel2-agent-fixture
    (:documentation "A fixture that instantiates a mongrel2 test agent."
                    :setup (progn
                             (start mongrel2-runner)
                             (unless (wait-for-agent-message (hypervisor-uuid :timeout 60) (msg)
                                       (awhen (getf msg :info)
                                         (getf it :peers)))
                               (error "Mongrel2 didn't start.")))
                    :cleanup (progn
                               (stop mongrel2-runner)
                               (with-agent-conversation (m e :timeout 5 :linger -1) mongrel2-uuid
                                   (zmq:send! e (prepare-message `(:agent :kill :kill ,mongrel2-uuid))))))

  (hypervisor-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (mongrel2-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (mongrel2-runner (make-runner :test :include '(:afdog-tests)
                                 :class 'afdog-hypervisor-test-agent
                                 :agents `(quote ( mongrel2-test-agent  (:uuid ,mongrel2-uuid)))
                                 :root *root* ;; different root for the test agents
                                 :uuid hypervisor-uuid)))

(def-fixtures afdog-hypervisor-agent-fixture
    (:documentation "A fixture that instantiates an afdog-hypervisor test agent."
                    :setup (progn
                             (start afdog-hypervisor-runner))
                    :cleanup (progn
                               (ignore-errors (iolib.syscalls:kill pid iolib.syscalls:sigkill))
                               (stop afdog-hypervisor-runner)))
  (pid nil)
  (afdog-hypervisor-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (afdog-hypervisor-runner (make-runner :test :include '(:afdog-tests)
                                :class 'afdog-hypervisor-test-agent
                                :root *root* ;; different root for the test agents
                                :uuid afdog-hypervisor-uuid)))

(defcategory request-processing-tests)
(def-fixtures request-processing-agent-fixture
    (:documentation "A fixture that instantiates a request processing agent."
     :setup (progn
              (start request-processing-runner)

              (when (and (boundp 'mongrel2-uuid) mongrel2-uuid)
                ;; Wait until we boot then tell the m2 agent about us
                (log-for (request-processing-tests trace) "Waiting for req-proc agent to boot to inform m2 about it.")
                (wait-for-agent (request-processing-uuid :timeout 60)
                  (tell-agent-about mongrel2-uuid request-processing-uuid))

                ;; Wait until we score some peers from talking to mongrel2
                (log-for (request-processing-tests trace) "Waiting for req-proc agent to get peers")
                (wait-for-agent-message (request-processing-uuid :timeout 60) (msg)
                  (awhen (getf msg :info)
                    (getf it :peers))))

              (log-for (request-processing-tests trace) "request-processing-agent-fixture :setup finished."))

     :cleanup (progn
                (stop request-processing-runner)))

  (request-processing-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (request-processing-runner (make-runner :test :include '(:afdog-tests)
                                          :handle "api"
                                          :class 'request-processing-test-agent
                                          :uuid request-processing-uuid)))

(defcategory api-tests)
(def-fixtures api-agent-fixture
    (:documentation "A fixture that instantiates an api agent."
     :setup (progn
              (start api-runner)
              (unless (wait-for-agent-message (hypervisor-uuid :timeout 60) (msg)
                        (awhen (getf msg :info)
                          (assoc api-uuid (getf it :peers) :test #'string=)))
                (error "API Agent didn't peer up."))
              (log-for (api-tests trace) "api-agent-fixture :setup finished."))

     :cleanup (progn
                (stop api-runner)))

  (hypervisor-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (api-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (mongrel2-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (api-runner (make-runner :test :include '(:afdog-tests)
                                 :class 'afdog-hypervisor-test-agent
                                 :agents `(quote ( mongrel2-test-agent  (:uuid ,mongrel2-uuid)
                                                   api-test-agent  (:uuid ,api-uuid)))
                                 :root *root* ;; different root for the test agents
                                 :uuid hypervisor-uuid)))

(def-fixtures request-forwarder-agent-fixture
    (:documentation "A fixture that instantiates an api agent."
     :setup (progn
              (start request-forwarder-runner)
              (unless (wait-for-agent-message (hypervisor-uuid :timeout 60) (msg)
                        (awhen (getf msg :info)
                          (and (assoc request-forwarder-uuid (getf it :peers) :test #'string=)
                               (assoc forwarder-agent-uuid (getf it :peers) :test #'string=))))
                (error "Request forwarder and forwarder config Agents didn't peer up."))

              (unless (wait-for-agent-message (forwarder-agent-uuid
                                               :request
                                               `(:agent :need
                                                        :need :forwarder
                                                        :forwarder (:name "test"
                                                                    :hosts ("api.example.com" "localhost")
                                                                    :routes (("default" . "/") ("one" . "/1/")))))
                          (msg)
                        (awhen (getf msg :filled)
                          (when (getf msg :forwarder)
                            :need-filled)))
                (error "Could not create forwarder.")))

     :cleanup (progn
                (stop request-forwarder-runner)))

  (hypervisor-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (request-forwarder-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (forwarder-agent-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (forwarder-handler "forwarder-test-default")
  (mongrel2-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (request-forwarder-runner (make-runner :test :include '(:afdog-tests)
                                         :class 'afdog-hypervisor-test-agent
                                         :agents `(quote ( mongrel2-test-agent  (:uuid ,mongrel2-uuid)
                                                           forwarder-test-agent (:uuid ,forwarder-agent-uuid)
                                                           request-forwarder-test-agent  (:uuid ,request-forwarder-uuid
                                                                                          :forwarder "test"
                                                                                          :route "default")))
                                         :root *root* ;; different root for the test agents
                                         :uuid hypervisor-uuid)))

(def-fixtures forwarder-agent-fixture
    (:documentation "A fixture that instantiates a mongrel2 test agent."
                    :setup (progn
                             (start forwarder-runner)
                             (log-for (trace) "Waiting for hypervisor to boot")
                             (wait-for-agent-message (hypervisor-uuid :timeout 60) (msg)
                               (awhen (getf msg :info)
                                 (getf it :peers)))

                             (log-for (trace) "Waiting for forwarder-agent to discover peers")
                             (wait-for-agent-message (forwarder-agent-uuid :timeout 60) (msg)
                               (awhen (getf msg :info)
                                 (getf it :peers))))
                    :cleanup (progn
                               (stop forwarder-runner)
                               (with-agent-conversation (m e :timeout 5 :linger -1) mongrel2-uuid
                                   (zmq:send! e (prepare-message `(:agent :kill :kill ,mongrel2-uuid))))
                               (with-agent-conversation (m e :timeout 5 :linger -1) forwarder-agent-uuid
                                   (zmq:send! e (prepare-message `(:agent :kill :kill ,forwarder-agent-uuid))))))

  (hypervisor-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (mongrel2-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (forwarder-agent-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (forwarder-runner (make-runner :test :include '(:afdog-tests)
                                 :class 'afdog-hypervisor-test-agent
                                 :agents `(quote ( mongrel2-test-agent  (:uuid ,mongrel2-uuid) forwarder-test-agent (:uuid ,forwarder-agent-uuid)))
                                 :root *root* ;; different root for the test agents
                                 :uuid hypervisor-uuid)))

(def-fixtures kill-everything-fixture
    (:documentation "A fixture that kills every process spawned by an agent"
     :cleanup (progn
                (afdog:kill-everything))))

(def-fixtures afdog-bin-fixture
    (:documentation "Pathname to afdog binary")
  (afdog-bin (merge-pathnames (make-pathname :directory '(:relative "bin") :name "afdog") afdog:*root*)))

(def-fixtures cli-agent-uuid-fixture
    (:documentation "Provides a uuid for cli agents.")
  (uuid (format nil "~A" (uuid:make-v4-uuid))))

(def-fixtures mongrel2-agent-cli-fixture
    (:documentation "Arguments for starting and stopping a mongrel2 agent")
  (afdog-start-args `("start" "mongrel2-agent" "-u" ,uuid))
  (afdog-kill-args `("kill" ,uuid)))

(def-fixtures afdog-hypervisor-agent-cli-fixture
    (:documentation "Arguments for starting and stopping a mongrel2 agent")
  (afdog-start-args `("start" "afdog-hypervisor-agent" "-u" ,uuid))
  (afdog-kill-args `("kill" ,uuid)))
