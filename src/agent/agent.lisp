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

(defgeneric start (runner &key category)
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
                 :initarg :options
                 :accessor runner-lisp-options)
   (init-forms :initform `((load ,(namestring (merge-pathnames "setup.lisp" ql:*quicklisp-home*)))
                           (ql:quickload :afdog))
               :initarg :init
               :accessor init-forms
               :documentation "Forms to get the lisp image ready to execute the runtime code.")
   (exec-form-prefix :initform "--eval"
                     :initarg :exec-prefix
                     :accessor exec-form-prefix
                     :documentation "An argument to add before every form on the command line.")
   (exec-forms :initform `()
               :accessor :exec
               :accessor exec-forms
               :documentation "Forms to cause the execution of the agent.")
   (terminate-forms :initform `((sb-ext:quit :unix-status 0))
                    :initarg :terminate
                    :accessor terminate-forms
                    :documentation "Forms to cause the death of this process after execution."))
  (:documentation "Load up a new interpreter, and follow some steps to load an agent as requested."))

(defcategory exec-runner)

(defmethod initialize-instance :after ((runner exec-runner) &rest initargs)
  (log-for (exec-runner trace) "Starting runner ~A" runner)
  (dolist (system (getf (getf initargs :initargs) :include))
    (log-for (exec-runner trace) "system ~A" system)
    (setf (init-forms runner)
          (append (init-forms runner)
                  `((ql:quickload ,system)))))

  (log-for (exec-runner trace) "Runner agent initforms ~S" (init-forms runner))
  (log-for (exec-runner trace) "Runner agent Initargs ~S" (runner-agent-initargs runner))

  (setf (exec-forms runner)
        (append (exec-forms runner)
                `((in-package ,(package-name (symbol-package (agent-instance runner))))
                  (start (make-runner :host :class (quote ,(agent-instance runner))
                                      ,@(runner-agent-initargs runner))))))
    (log-for (exec-runner trace) "Runner agent execforms ~S" (exec-forms runner)))

(defmethod make-runner ((style (eql :exec)) &rest initargs &key (class 'standard-agent) &allow-other-keys)
  (log-for (warn) "Exec runner initargs: ~A" initargs)
  (mapc  #'(lambda (key) (remf initargs key)) '(:class :agent :include))
  (log-for (warn) "Exec runner initargs after prune: ~A" initargs)
  (make-instance 'exec-runner :agent class
                 :initargs initargs))

(defmethod start ((runner exec-runner) &key (category '(log5:dribble+)))
  "Starts a runner by starting a new lisp."
  (unless (running-p runner)
    (flet ((prepare-forms (forms)
             (loop for form in forms appending
                  (remove nil
                          (list (exec-form-prefix runner)
                                (with-output-to-string (s) (prin1 form s)))))))

      (setf (agent-handle runner)
            (afdog:run-program (runner-lisp runner)
                               `(,@(runner-lisp-options runner)
                                  ,@(prepare-forms (init-forms runner))
                                  ,@(prepare-forms (exec-forms runner))
                                  ,@(prepare-forms (terminate-forms runner)))
                               :wait nil))
      runner)))

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
;; Blocked runner, for operation within the current thread.
;;
(defclass blocked-runner (agent-runner)
  ())

(defmethod make-runner ((style (eql :blocked)) &key)
  (let ((runner (call-next-method)))
    (change-class runner 'blocked-runner)))

(defmethod start ((runner blocked-runner) &key (category '(log5:dribble+)))
  (prog1 runner
    (start-logging :category category)

    (setf (agent-handle runner) (agent-instance runner))
    (unwind-protect (run-agent (agent-handle runner))
      (setf (agent-handle runner) nil))))
