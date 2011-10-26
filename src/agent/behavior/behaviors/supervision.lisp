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

(defbehavior create-links (:on (:command :link :from :head) :include (link-manager) :do :invoke-with-event) (organ event)
  ;; (:command :link
  ;;           :link :agent
  ;;           :agent (:uuid ,uuid :class leaf-test-agent :package :afdog-tests)
  ;;   --or --
  ;;           :process (:pid pid :make (:cmd "string" :args ("list" "of" "strings") :pwd ""))
  ;;           .. or something

  ;; TODO: Not this, do the real thing, damnit
  (let* ((link-what (getf event :link))
         (link (and link-what
                    (getf event link-what))))

    (and link-what link
         (send-message organ :command `(:command :speak
                                                 :debug t
                                                 :say (:spawning :what ,link-what :with ,link
                                                                 :key ,(link-key link-what link)))))))


(defgeneric link-key (what info)
  (:documentation "Generate a hash table string key for the thing described by `what' and `info'")
  (:method (what info) "Default operation is a noop" nil)

  (:method ((what (eql :agent)) (info list))
    "Generates an agent-uuid hash key"
    (let ((uuid (getf info :uuid)))
      (and uuid
           (format nil "agent-~A" uuid)))))
