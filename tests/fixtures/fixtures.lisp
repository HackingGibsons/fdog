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
                    :cleanup (progn
                               (ignore-errors (clsql:disconnect) (fdog-models:disconnect))
                               (sb-ext:delete-directory db-root :recursive t)))
  (db-root (merge-pathnames (make-pathname :directory fdog-models:*server-dir*)
                            *root*))
  (db-path (merge-pathnames fdog-models:*config-file*
                            db-root))
  (connected-p nil))

(def-fixtures mongrel2-agent-fixture
    (:documentation "A fixture that instantiates a mongrel2 test agent."
                    :setup (progn
                             (start mongrel2-runner)
                             (unless (with-agent-conversation (m e :timeout 60) hypervisor-uuid
                                       (do* ((msg (parse-message (read-message m))
                                                  (parse-message (read-message m)))
                                             (info (getf msg :info) (getf msg :info))
                                             (peers (getf info :peers) (getf info :peers)))
                                            (peers t)))
                               (error "Mongrel2 didn't start.")))
                    :cleanup (progn
                               (ignore-errors (iolib.syscalls:kill pid iolib.syscalls:sigkill))
                               (stop mongrel2-runner)))
  (pid nil)
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

(def-fixtures kill-everything-fixture
    (:documentation "A fixture that kills every process spawned by an agent"
                    :cleanup (afdog:kill-everything)))

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
