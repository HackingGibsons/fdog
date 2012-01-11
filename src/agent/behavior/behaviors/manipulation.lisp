(in-package :agent)

(defbehavior make-things (:on (:command :make :from :head) :do :invoke-with-event) (organ event)
  ;; (:head :command
  ;;  :command :make
  ;;  :make :agent
  ;;  :agent (:uuid uuid :class standard-leaf-agent :package :agent))
  (let ((what (getf event :make)))
    (make-item behavior what (getf event what))))

(defgeneric make-item (behavior what info)
  (:documentation "Generic protocol for construction of things.")
  (:method (behavior what info)
    nil))

(defmethod make-item ((behavior make-things) (what (eql :agent)) info)
  (log-for (watch-machine trace) "Spawn agent: ~A" info)
  ;; TODO: Figure out slightly better how to select the spawn class
  (let* ((package (find-package (getf info :package)))
         (uuid (getf info :uuid)))

    ;; At this point we have assured that `class' is a real class
    ;; and is a subclass of `standard-agent'
    (when package
      ;; Find a spawner in the package of the agent class or use the default
      (let* ((spawner-sym (or (find-symbol (symbol-name '*spawner*) (package-name package))
                              '*spawner*))
             (spawner (symbol-value spawner-sym))

             (initargs `(,@info 
                        :uuid ,uuid
                        :parent-uuid ,(agent-uuid (organ-agent (behavior-organ behavior)))
                        :parent-mouth ,(mouth-addr (find-organ (organ-agent (behavior-organ behavior)) :mouth))))

             (runner (apply #'make-runner spawner initargs)))

        (and runner
             (start runner))
        (send-message (behavior-organ behavior) :made `(:made ,what ,what ,(getf info what)))))))

(defmethod make-item ((behavior make-things) (what (eql :process)) info)
  (let* ((path (getf info :path))
         (args (getf info :args))
         (trans-id (getf info :transaction-id))
         (process (afdog:run-program path args :wait nil))
         (pid (and process (sb-ext:process-pid process))))
    (log-for (trace watch-machine) "made process - path: ~A args: ~A transaction-id: ~A pid: ~A" path args trans-id pid)
    (send-message (behavior-organ behavior) :made `(:made ,what ,what (:pid ,pid :transaction-id ,trans-id)))))
