(in-package :agent)

(defbehavior make-things (:on (:command :make :from :head) :do :invoke-with-event) (organ event)
  ;; (:head :command
  ;;  :command :make
  ;;  :make :agent
  ;;  :agent (:uuid uuid :class standard-leaf-agent :package :agent)))
  (let ((what (getf event :make)))
    (make-item behavior what (getf event what))
    (send-message organ :made `(:made ,what ,what ,(getf event what)))))

(defgeneric make-item (behavior what info)
  (:documentation "Generic protocol for construction of things.")
  (:method (behavior what info)
    nil))

(defmethod make-item ((behavior make-things) (what (eql :agent)) info)
  (format t "Spawn agent: ~A~%" info)
  ;; TODO: Figure out slightly better how to select the spawn class
  (let* ((package (find-package (getf info :package)))

         (class (and package (getf info :class)))
         (class (and class (find-symbol (symbol-name class) package)))
         (class (and class (handler-case (find-class class)
                             (simple-error () nil))))

         (uuid (getf info :uuid)))

    ;; At this point we have assured that `class' is a real class
    ;; and is a subclass of `standard-agent'
    (when (and class package)
      ;; Find a spawner in the package of the agent class or use the default
      (let* ((spawner-sym (or (find-symbol (symbol-name '*spawner*) (package-name package))
                              '*spawner*))
             (spawner (symbol-value spawner-sym))

             (initargs `(:uuid ,uuid
                               :parent-uuid ,(agent-uuid (organ-agent (behavior-organ behavior)))
                               :parent-mouth ,(mouth-addr (find-organ (organ-agent (behavior-organ behavior)) :mouth))))
             (runner (apply #'make-runner spawner :class (class-name class) initargs)))

        (and runner
             (start runner))))))
