(in-package :afdog-tests)

;; Runner encapsulation
(defclass test-runner (agent::exec-runner) ())

(defmethod update-instance-for-different-class :after (previous (current test-runner) &key)
  (setf (agent::init-forms current)
        (append (agent::init-forms current)
                '((ql:quickload :afdog-tests)))))

(defmethod agent::make-runner ((style (eql :test)) &rest keys &key)
  (change-class (apply #'agent::make-runner :exec keys) 'test-runner))

;; Test fixtures
(def-fixtures agent-fixture
    (:special (agent))
  (agent (make-instance 'test-agent)))

(def-fixtures running-hypervisor-fixture
    (:setup
     (agent::start agent-runner)

     :cleanup
     (agent::stop agent-runner))

  (agent-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (agent-runner (agent::make-runner :test :include '(:afdog-tests)
                                    :class 'hypervisor-test-agent
                                    :uuid agent-uuid)))

(def-fixtures running-hypervisor-child
    (:setup (agent::start child-runner)
     :cleanup (agent::stop child-runner))

  (child-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (child-runner (agent::make-runner :test :include '(:afdog-tests)
                                    :class 'leaf-test-agent
                                    :uuid child-uuid
                                    :parent-uuid agent-uuid
                                    :parent-mouth (agent::local-ipc-addr agent-uuid :mouth))))

(def-fixtures started-parent-and-child
    (:setup (progn (agent::start parent)
                   (agent::start child))

     :cleanup (progn (agent::stop parent)
                     (agent::stop child)))

  (uuid (format nil "~A" (uuid:make-v4-uuid)))
  (kid-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (parent (agent::make-runner :test :include '(:afdog-tests)
                              :class 'hypervisor-test-agent
                              :uuid uuid))
  (child (agent::make-runner :test :include '(:afdog-tests)
                             :class 'leaf-test-agent
                             :uuid kid-uuid
                             :parent-uuid uuid
                             :parent-mouth (agent::local-ipc-addr uuid :mouth))))

(def-fixtures running-agent-fixture
    (:setup
     (unless (agent::running-p agent-runner)
       (agent::start agent-runner))

     :cleanup
     (if (agent::running-p agent-runner)
         (agent::stop agent-runner)))

  (agent-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (agent-runner (agent::make-runner :test :include '(:afdog-tests)
                                    :class 'afdog-tests::runner-agent
                                    :uuid agent-uuid)))
