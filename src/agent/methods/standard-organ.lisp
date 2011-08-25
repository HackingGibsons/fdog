(in-package :agent)

(defmethod agent-tick ((organ standard-organ) event)
  "By default, an organ tick is a no-op"
  nil)
