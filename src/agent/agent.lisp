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

(defgeneric make-runner (style &key class &allow-other-keys)
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
;; A runner that executes and external lisp and loads code to boot the agent
;;
(defclass exec-runner (agent-runner)
  ((agent-initargs :initform () :initarg :initargs
                   :accessor runner-agent-initargs)
   (lisp :initform sb-ext:*runtime-pathname*
         :accessor runner-lisp
         :initarg :lisp)
   (lisp-options :initform '("--disable-debugger")
                 :accessor runner-lisp-options)
   (init-forms :initform `((load ,(namestring (merge-pathnames "setup.lisp" ql:*quicklisp-home*)))
                           (ql:quickload :afdog))
               :accessor init-forms
               :documentation "Forms to get the lisp image ready to execute the runtime code.")
   (exec-forms :initform `()
               :accessor exec-forms
               :documentation "Forms to cause the execution of the agent.")
   (terminate-forms :initform `((sb-ext:quit :unix-status 0))
                    :accessor terminate-forms
                    :documentation "Forms to cause the death of this process after execution."))
  (:documentation "Load up a new interpreter, and follow some steps to load an agent as requested."))

(defmethod initialize-instance :after ((runner exec-runner) &rest initargs)
  (declare (ignorable initargs))
  (setf (exec-forms runner)
        (append (exec-forms runner)
                `((in-package ,(package-name (symbol-package (agent-instance runner))))
                  (start (make-runner :blocked :class (quote ,(agent-instance runner))
                                      ,@(runner-agent-initargs runner)))))))

(defmethod make-runner ((style (eql :exec)) &rest initargs &key (class 'standard-agent) &allow-other-keys)
  (log-for (warn) "Exec runner initargs: ~A" initargs)
  (labels ((remove-from-plist (list key &rest keys)
             (reduce #'(lambda (acc b) (remove-from-plist acc b)) keys
                     :initial-value (cond ((null list) nil)
                                          ((equalp (car list) key) (remove-from-plist (cddr list) key))
                                          (t (append (list (first list) (second list)) (remove-from-plist (cddr list) key)))))))
    (make-instance 'exec-runner :agent class :initargs (remove-from-plist initargs :class :agent))))

(defmethod start ((runner exec-runner))
  "Starts a runner by starting a new lisp."
  (unless (running-p runner)
    (flet ((prepare-forms (forms)
             (loop for form in forms appending
                  (list "--eval"
                        (with-output-to-string (s) (prin1 form s))))))

      (setf (agent-handle runner)
            (list 'sb-ext:run-program (runner-lisp runner)
                                `(,@(runner-lisp-options runner)
                                  ,@(prepare-forms (init-forms runner))
                                  ,@(prepare-forms (exec-forms runner))
                                  ,@(prepare-forms (terminate-forms runner)))
                                :wait nil)))))

(defmethod running-p ((runner exec-runner))
  (and (agent-handle runner)
       (sb-ext:process-p (agent-handle runner))
       (sb-ext:process-alive-p (agent-handle runner))))

(defmethod stop ((runner exec-runner))
  (when (running-p runner)
    (sb-ext:process-kill (agent-handle runner) iolib.syscalls:sigquit)
    (handler-case (bt:with-timeout (1)
                    (sb-ext:process-wait (agent-handle runner) t))
      (bt:timeout () :timeout))))


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
