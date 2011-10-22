(in-package :agent)

;; Organ health check
(defmethod act-on-event ((head agent-head) event)
  (prog1 event
    (cond ((and (listp event) (getf (reverse event) :beat))
           (organ-beat-event head event))
          (:otherwise
           (log-for (warn) "~A doesn't deal with ~A" head event)))))

(defmethod organ-beat-event ((head agent-head) (event list))
  "Handle the updating of an organ's state."
  (let* ((tag (car event))
         (uuid (getf event :uuid))
         (time (getf event :time))
         (status `(:tag ,tag :time ,time)))

    (unless (and tag uuid time)
      (log-for (warn) "~A: organ-beat-event did nat have all of: tag, :uuid or :time" head)
      (error "organ-beat-event did nat have all of: tag, :uuid or :time"))

    (log-for (trace) "Updating organ: ~A/~A @ ~A" tag uuid time)
    (setf (gethash uuid (agent-organs head)) status)))

(defmethod organ-beat-event :after ((head agent-head) (event list))
  "Handle organ eviction after tick processing"
  (with-hash-table-iterator (organ-iter (agent-organs head))
    (loop (multiple-value-bind (more? uuid status) (organ-iter)
            (unless more? (return nil))
            (unless (organ-healthy-p head uuid status)
              (suicide head))))))

(defmethod organ-healthy-p ((head agent-head) uuid (status list))
  "Check the status of the organ as a predicate"
  (log-for (trace) "~A checking health of ~A using ~A" head uuid status)
  (let ((cutoff (or (car (last (last-beat head))) 0)))
    (log-for (trace) "Setting death cutoff to be <~A" cutoff)
    (if (< (or (getf status :time) 0) cutoff)
        (prog1 nil (log-for (warn) "~A/~A appears dead." uuid status))
        (prog1 t (log-for (trace) "~A/~A appears alive." uuid status)))))

;; Death
(defmethod suicide ((agent standard-agent))
  "Suicide an agent rather than a head."
  (suicide (find-organ agent :head)))

(defmethod suicide ((head agent-head))
  "Send a death message down the bus, agent should terminate."
  (log-for (warn) "~A/~A I HAVE LOST THE WILL TO LIVE!" (organ-agent head) head)
  (send-message head :command `(:command :die
                                :uuid ,(organ-uuid head)
                                :time ,(get-internal-real-time))))

;; Peer maintenence
(defmethod update-peer ((head agent-head) peer-info)
  "Update or store information about a peer we have heard about."
  (let ((uuid (getf peer-info :uuid))
        (ear (getf peer-info :ear))
        (mouth (getf peer-info :mouth)))
    (unless (and uuid ear mouth)
      (log-for (warn) "Info did not contain a UUID mouth or ear.")
      (return-from update-peer))
    (log-for (warn) "Storing info on ~A => ~A/~A" uuid ear mouth)
    (setf (gethash uuid (agent-peers head))
          `(:time ,(get-internal-real-time) ,@peer-info))))

(defmethod evict-old-peers ((head agent-head))
  :TODO)

(defmethod update-peer :after ((head agent-head) peer-info)
  "Walk all of the peers we have and listen to each of them."
  (evict-old-peers head)

  (flet ((listen-to (uuid peer)
           (declare (ignorable uuid))
           (let ((listen-addr (getf (getf peer :mouth) :addr)))
             (when listen-addr
               (send-message head :command `(:command :listen
                                             :uuid ,(organ-uuid head)
                                             :listen ,listen-addr))))))
    (maphash #'listen-to (agent-peers head))))


(defmethod heard-message ((head agent-head) (from (eql :agent)) (type (eql :info)) &rest info)
  "Agent info hearing and storing."
  (let ((info (getf info :info)))
    (update-peer head info)))

(defmethod agent-info ((head agent-head))
  (let (acc)
    (maphash #'(lambda (uuid peer)
                 (push `(,uuid . (:ear ,(getf (getf peer :ear) :addr)
                                  :mouth ,(getf (getf peer :mouth) :addr)))
                       acc))
             (agent-peers head))
    `(:peers ,acc)))
