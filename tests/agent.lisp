(in-package :afdog-tests)

(defclass test-agent (agent::standard-agent) ())

(defun make-test-agent ()
  (make-instance 'test-agent))

(defun make-test-runner ()
  (let ((test-agent (make-test-agent)))
    (flet ((test-thread ()
             (agent::run-agent test-agent)))
      (values (lambda () (bt:make-thread #'test-thread :name "test-thread")) test-agent))))

(def-test (can-test-nothing :group basic-tests)
    (:true)
  t)

(def-test (events-not-zero :group basic-tests)
    (:true)
  (multiple-value-bind (runner agent) (make-test-runner)
    (let ((thread (funcall runner)))
      (sleep 0.1)
      (bt:destroy-thread thread)
      (describe agent)
      (not (zerop (agent::agent-event-count agent))))))

    