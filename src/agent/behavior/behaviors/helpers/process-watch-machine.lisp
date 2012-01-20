(in-package :agent)
;; Process specific watch machine
(defcategory process-watch-machine)

(defclass process-watch-machine (standard-watch-machine)
  ((key :initform nil :initarg :key :accessor key))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "A specialization of the `standard-watch-machine' that knows how to create processes"))

(defstate process-watch-machine :initial (info)
  "An 'initial' agent for 'process-watch-machine', asks for the construction of a process and transitions to the `:made' state"
  (log-for (process-watch-machine trace watch-machine) "Running :initial event of ~A" machine)
  (log-for (process-watch-machine trace watch-machine) "Thing info: ~A" (thing-info machine))
  (setf (getf (timestamps machine) :made) nil)
  (let ((transaction (format nil "~A" (uuid:make-v4-uuid))))
    (setf (gethash transaction (transactions (behavior machine))) (key machine))
    (log-for (process-watch-machine trace watch-machine) "Trans storage: ~A" (transactions (behavior machine)))

    (send-message (behavior-organ (behavior machine)) :command
                  `(:command :make
                             :make :process
                             :process ,(concatenate 'list
                                                    (thing-info machine)
                                                    `(:transaction-id ,transaction)))))
  :made)

(defstate process-watch-machine :made (info)
  (log-for (process-watch-machine trace watch-machine) ":made state of ~A => ~A" machine info)
  (let ((pid (cond ((and (eql (car info) :event) (eql (getf info :event) :boot))
                    (getf (thing-info machine) :pid))
                   (t
                    (getf info :pid)))))
    (if (not pid)
        (prog1 nil
          (log-for (process-watch-machine watch-machine trace)  ":made state wants to run with no pid. Event: ~A" info))

        (progn
          (send-message (behavior-organ (behavior machine)) :command `(:command :watch
                                                                                :watch (:process :pid :pid ,pid)))
          (setf (gethash pid (pids (behavior machine))) (key machine))
          (call-next-method)))))

(defstate process-watch-machine :watch (info)
  (log-for (process-watch-machine watch-machine trace) "In :watch state of ~A" machine)
  (unless (getf info :alive)
    (log-for (process-watch-machine warn watch-machine) "process has died")
    :died))

(defstate process-watch-machine :died (info)
  (log-for (process-watch-machine watch-machine trace) "In :died state of ~A" machine)
  (let ((pid (getf info :pid)))
    (remhash pid (pids (behavior machine)))
    (log-for (process-watch-machine watch-machine trace) "Stopping the old watch of ~A" pid)
    (send-message (behavior-organ (behavior machine)) :command `(:command :stop-watching
                                                                 :stop-watching (:process :pid :pid ,pid))))
  (values (call-next-method)
          :fire-again))

