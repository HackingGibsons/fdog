(in-package :agent)

(defbehavior make-things (:on (:command :make :from :head) :do :invoke-with-event) (organ event)
  (declare (ignorable organ))
  ;; (:head :command
  ;;  :command :make
  ;;  :make :agent
  ;;  :agent (:uuid uuid :class standard-leaf-agent :package :agent)))
  (let ((what (get event :make)))
    (make-item behavior what (getf event what))))

(defgeneric make-item (behavior what info)
  (:documentation "Generic protocol for construction of things.")
  (:method ((behavior standard-behavior) what info)
    nil))

(defmethod make-item ((behavior make-things) (what (eql :agent)) info)
  (format t "Spawn agent: ~A~%" info)
  ;; TODO: Figure out slightly better how to select the spawn class
  (let* ((package (find-package (getf info :package)))

         (class (and package (getf info :class)))
         (class (and class (find-symbol (symbol-name class) package)))
         (class (and class (handler-case
                               (prog1 (find-class class)
                                 (c2mop:finalize-inheritance (find-class class)))
                             (simple-error () nil))))
         (class (and class
                     (find (find-class 'standard-agent)
                           (c2mop:class-precedence-list class))
                     class))

         (uuid (getf info :uuid)))

    ;; At this point we have assured that `class' is a real class
    ;; and is a subclass of `standard-agent'
    (when (and class package)
      (let* ((initargs `(:uuid ,uuid
                         :parent-uuid ,(agent-uuid (organ-agent (behavior-organ behavior)))
                         :parent-mouth ,(mouth-addr (find-organ (organ-agent (behavior-organ behavior)) :mouth))))
             (runner (apply #'make-runner *spawner* :class (class-name class) initargs)))

        (and runner
             (start runner))))))
