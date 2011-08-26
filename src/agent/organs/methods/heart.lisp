(in-package :agent)

(defmethod agent-tick ((heart agent-heart) e)
  (declare (ignorable e))
  (let ((now (get-internal-real-time)))
    (when (>= now (heart-next-beat heart))
      (send-message heart `(:heart :beat :uuid ,(organ-uuid heart) :time ,now))

      (setf (heart-last-beat heart) now
            (heart-next-beat heart) (round (+ now (* (heart-beat-every heart)
                                                     internal-time-units-per-second)))))
    (heart-next-beat heart)))


