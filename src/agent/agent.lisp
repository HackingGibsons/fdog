(in-package :agent)

;; Base agent runner
(defclass agent-runner ()
  ((agent :initform nil :initarg :agent
          :reader agent-instance
          :documentation "Agent instance")
   (handle :initform nil
           :accessor agent-handle
           :documentation "Handle to whatever ends up running the agent. Should be non-nil during operation."))
  (:documentation "Base class for agent runners, only tracks an instance and abstract `handle'"))

(defgeneric make-runner (style &key)
  (:documentation "Make an agent running wrapper of the desired `style'.
Style specializations should (call-next-method) to get a base isntance, then (change-class ..) the
result into the desired type.")

  (:method (style &rest initargs &key (class 'standard-agent) &allow-other-keys)
    "Make the basic instance that is upgraded by other makers."
    (let ((agent (apply #'make-instance class :allow-other-keys t initargs)))
      (make-instance 'agent-runner :agent agent))))

(defgeneric start (runner)
  (:documentation "Start the `runner' by invoking `run-agent' on the enclosed agent"))
(defgeneric stop (runner)
  (:documentation "Stop the running agent in the container."))
(defgeneric running-p (runner)
  (:documentation "Predicate of if the given runner is currently running an agent.")
  (:method ((runner agent-runner))
    "Default running state is the presense of the handle."
    (agent-handle runner)))
;;
;; A runer that runs the given agent in a thread of the current process.
;;
(defclass thread-runner (agent-runner)
  ())

(defmethod make-runner ((style (eql :thread)) &key)
  (let ((runner (call-next-method)))
    (change-class runner 'thread-runner)))

(defmethod start ((runner thread-runner))
  (unless (running-p runner)
    (setf (agent-handle runner)
          (bt:make-thread #'(lambda () (run-agent (agent-instance runner)))))
    runner))


(defmethod running-p ((runner thread-runner))
  (with-accessors ((handle agent-handle)) runner
    (and handle
         (bt:threadp handle)
         (bt:thread-alive-p handle))))

(defmethod stop ((runner thread-runner))
  (when (running-p runner)
    (bt:destroy-thread (agent-handle runner))
    (setf (agent-handle runner) nil)
    runner))


;;
;; A runner that forks a child process to run the agent
;;
(defclass proc-runner (agent-runner)
  ())

(defmethod make-runner ((style (eql :proc)) &key)
  (change-class (call-next-method) 'proc-runner))

(defmethod running-p ((runner proc-runner))
  (handler-case
      (and (agent-handle runner)
           (iolib.syscalls:kill (agent-handle runner) 0)
           (agent-handle runner))

    (iolib.syscalls:ESRCH () nil)))

(defmethod start ((runner proc-runner))
  "Forking starter. Does not work in multithreaded sbcl."
  (unless (running-p runner)
    (let ((child  (handler-case (sb-posix:fork) (t () nil))))
      (case child
        (nil nil)
        (-1 (log-for (warn) "~A: FORK FAILED!" runner)
            nil)
        (0
         (setf (agent-handle runner) (iolib.syscalls:getpid))
         (unwind-protect (run-agent (agent-instance runner))
           (setf (agent-handle runner) nil)
           (iolib.syscalls:exit 0)))
        (t
         (setf (agent-handle runner) child))))))

(defmethod stop ((runner proc-runner))
  (when (running-p runner)
    (prog1 (agent-handle runner)
      (iolib.syscalls:kill (agent-handle runner) iolib.syscalls:sigkill)

      (handler-case (bt:with-timeout (1)
                      (iolib.syscalls:waitpid (agent-handle runner) 0))

        (bt:timeout () nil)
        (iolib.syscalls:echild () nil))

      (setf (agent-handle runner) nil))))

;;
;; Blocked runner, for operation within the current thread.
;;
(defclass blocked-runner (agent-runner)
  ())

(defmethod make-runner ((style (eql :blocked)) &key)
  (let ((runner (call-next-method)))
    (change-class runner 'blocked-runner)))

(defmethod start ((runner blocked-runner))
  (prog1 runner
    (setf (agent-handle runner) (agent-instance runner))
    (unwind-protect (run-agent (agent-handle runner))
      (setf (agent-handle runner) nil))))
