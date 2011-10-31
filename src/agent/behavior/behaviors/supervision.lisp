(in-package :agent)

(defclass lonely-mixin ()
  ((last-seen :initform (get-internal-real-time)
              :accessor last-seen)
   (lonely-tolerance :initform (* 10 internal-time-units-per-second)
                     :accessor lonely-tolerance)))

;; TODO: Maybe something more like (:need (:agent :uuid :from #'parent-uuid) :do invoke) ?
(defbehavior die-without-parent (:interval (:from :heart :nth 6) :include (lonely-mixin) :do :invoke) (organ)
  (let ((parent (gethash (parent-uuid (organ-agent organ)) (agent-peers organ)))
        (now (get-internal-real-time)))

    (when parent
      (setf (last-seen behavior) (getf parent :time 0)))

    (when (<= (lonely-tolerance behavior)
              (- now (last-seen behavior)))
      (log-for (warn) "~A: My parent has died." organ)
      (suicide (organ-agent organ)))))

(defclass link-manager ()
  ((links :initform (make-hash-table :test 'equalp)
          :accessor links)))

(defgeneric link-key (behavior what info)
  (:documentation "Generate a hash table string key for the thing described by `what' and `info'")
  (:method (b what info) "Default operation is a noop" nil)

  (:method ((behavior link-manager) (what (eql :agent)) (info list))
    "Generates an agent-uuid hash key"
    (let ((uuid (getf info :uuid)))
      (and uuid
           (format nil "agent-~A" uuid)))))

(defgeneric link-init (behavior what info)
  (:method (b w i) nil))

(defgeneric link-event (behavior what info)
  (:method (b w i) nil))

(defbehavior create-links (:or ((:on (:command :link :from :head))
                                (:on (:saw :process :from :eye))
                                (:on (:saw :agent :from :eye)))
                               :include (link-manager) :do :invoke-with-event) (organ event)
  ;; (:command :link
  ;;           :link :agent
  ;;           :agent (:uuid ,uuid :class leaf-test-agent :package :afdog-tests)
  ;;   --or --
  ;;           :link :process
  ;;           :process (:pid pid :make (:cmd "string" :args ("list" "of" "strings") :pwd ""))
  ;;           .. or something

  ;; TODO: Not this, write a dispawtching :do for compound events
  (cond
    ((getf event :saw)
     (create-links-saw behavior organ event))

    ((getf event :link)
     (create-links-link behavior organ event))))

(defmethod create-links-saw ((behavior create-links) (organ standard-organ) event)
  (let* ((saw-what (getf event :saw))
         (saw-info (and saw-what
                        (getf event saw-what))))
    (link-event behavior saw-what saw-info)))

(defmethod create-links-link ((behavior create-links) (organ standard-organ) event)
  (let* ((link-what (getf event :link))
         (link-info (and link-what
                    (getf event link-what))))
    (link-init behavior link-what link-info)))


;; Agent init and event
(defmethod link-init ((behavior link-manager) (what (eql :agent)) info)
  (let ((key (link-key behavior what info)))
    (multiple-value-bind (value foundp) (gethash key (links behavior))
      (declare (ignorable value))
      (unless foundp
        ;; Store a state under the generated key
        (setf (gethash key (links behavior))
              `(:state :initial :time ,(get-internal-real-time) :what ,what :how ,info))

        ;; Make an agent
        (send-message (behavior-organ behavior) :command `(:command :make
                                                :make :agent
                                                :agent ,info))

        ;; Watch an agent
        (send-message (behavior-organ behavior) :command `(:command :watch
                                                :watch (:agent :uuid :uuid ,(getf info :uuid))))))))


(defmethod link-event ((behavior link-manager) (what (eql :agent)) info)
  (let (key (link-key behavior what info))
    (multiple-value-bind (value foundp) (gethash key (links behavior))
      (when foundp
        (format t "Found: ~A~%" value)
        :TODO))))
