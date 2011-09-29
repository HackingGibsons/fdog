(in-package :agent)

(defbehavior announce-self (:interval (:from :heart :nth 3) :do :invoke) (organ)
  (log-for (warn) "TODO: Running behavior lambda for ~A" organ)
  :oh-hello)
