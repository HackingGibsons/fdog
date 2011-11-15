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

(def-fixtures pid-fixture
  (:cleanup (progn
              (ignore-errors (iolib.syscalls:kill pid iolib.syscalls:sigkill))
              (ignore-errors (iolib.syscalls:kill old-pid iolib.syscalls:sigkill)))
   :documentation "A fixture to hold a pid that needs to be killed.
Does kill -9 to ensure the process dies in cleanup.")
  (old-pid)
  (pid))

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
