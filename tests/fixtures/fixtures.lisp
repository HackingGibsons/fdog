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
  (:cleanup (iolib.syscalls:kill pid iolib.syscalls:sigkill)
   :documentation "A fixture to hold a pid that needs to be killed.
Does kill -9 to ensure the process dies in cleanup.")
  (pid))
