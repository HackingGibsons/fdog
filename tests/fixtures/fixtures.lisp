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
   :documentation "A fixture to hold two pids for comparison")
  (old-pid)
  (pid))

(def-fixtures running-process-fixture
 (:cleanup (ignore-errors (iolib.syscalls:kill process-pid iolib.syscalls:sigkill))

  :documentation "A fixture that holds a running process.
Does kill -9 to ensure the process dies in cleanup.")

  (process (afdog:run-program "/usr/bin/yes" `(,(prin1-to-string (uuid:make-v4-uuid))) :wait nil))
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
                    :cleanup (sb-ext:delete-directory db-root :recursive t))
  (db-root (merge-pathnames (make-pathname :directory fdog-models:*server-dir*)
                            *root*))
  (db-path (merge-pathnames fdog-models:*config-file*
                            db-root))
  (connected-p nil))

(def-fixtures mongrel2-agent-fixture
    (:documentation "A fixture that instantiates a mongrel2 test agent."
                    :setup (progn
                             (start mongrel2-runner))
                    :cleanup (progn
                               (ignore-errors (iolib.syscalls:kill pid iolib.syscalls:sigkill))
                               (stop mongrel2-runner)))
  (pid nil)
  (mongrel2-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (mongrel2-runner (make-runner :test :include '(:afdog-tests)
                                :class 'mongrel2-test-agent
                                :root *root* ;; different root for the test agents
                                :uuid mongrel2-uuid)))
