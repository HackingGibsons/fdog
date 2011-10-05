(in-package :fdog-forwarder)

(clsql:def-view-class fdog-forwarder-queue ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key
       :reader fdog-forwarder-queue-id)
   (forwarder-id :type integer
                 :initarg :forwarder-id
                 :reader fdog-forwarder-queue-forwarder-id)
   (enabled :type boolean
            :accessor forwarder-queue-enabled
            :initarg :enabled
            :initform t)
   (depth :type integer
          :accessor forwarder-queue-depth
          :initarg :limit
          :initform nil))
  (:base-table fdog-forwarder-queue
   :documentation "Any applicable queue options for a given forwarder."))

(defmethod print-object ((object fdog-forwarder-queue) s)
  (format s "#<ForwarderQueue[~A]: Forwarder[~A] ~:[Disabled~;Enabled~] Depth: ~A>"
          (if (slot-boundp object 'id) (fdog-forwarder-queue-id object) "None")
          (if (slot-boundp object 'forwarder-id) (fdog-forwarder-queue-forwarder-id object) "None")
          (forwarder-queue-enabled object) (forwarder-queue-depth object)))

;; Queue related
(defmethod make-forwarder-queue-option ((forwarder fdog-forwarder))
  "Fetch an existing, or make a new `fdog-forwarder-queue' object
for a given `forwarder'"
  (clsql:update-objects-joins `(,forwarder))
  (let ((q-option (or (fdog-forwarder-queue-options forwarder)
                      (make-instance 'fdog-forwarder-queue
                                     :forwarder-id (fdog-forwarder-id forwarder)))))
    (setf (fdog-forwarder-queue-options forwarder) q-option)
    (clsql:update-records-from-instance q-option)
    q-option))

(defmethod fdog-forwarder-queue-options :around ((forwarder fdog-forwarder))
  "Ensure the freshest copy of the `forwarder' coming out when this method is called.
TODO: Paranoid precaution smells."
  (let ((q-opt (call-next-method)))
    (when q-opt
      (clsql:update-instance-from-records q-opt))
    q-opt))

(defmethod forwarder-queue-enable ((forwarder fdog-forwarder) &key depth)
  "Enable queueing on this `forwarder', next init requests will flow into redis."
  (log-for (trace) "Enabling request queue for ~A" forwarder)
  (let ((q-opt (make-forwarder-queue-option forwarder)))
    (setf (forwarder-queue-enabled q-opt) t
          (forwarder-queue-depth q-opt) depth)
    (clsql:update-records-from-instance q-opt))
  forwarder)


(defmethod forwarder-queue-disable ((forwarder fdog-forwarder))
  "Turn off queueing on this `forwarder', next init requests will no
longer flow to redis.
TODO: Consider what happens to outstanding queue requests."
  (log-for (trace) "Disabling request queueing for ~A" forwarder)
  (let ((q-opt (make-forwarder-queue-option forwarder)))
    (setf (forwarder-queue-enabled q-opt) nil)
    (clsql:update-records-from-instance q-opt))
  forwarder)

(defmethod forwarder-queuing-p ((forwarder fdog-forwarder))
  "Is the our `forwarder' queuing?"
  (let ((q-opt (make-forwarder-queue-option forwarder)))
    (and (forwarder-queue-enabled q-opt)
         (or (not (forwarder-queue-depth q-opt))
             (> 0 (forwarder-queue-depth q-opt))))))
