(in-package :fdog-models)

(clsql:def-view-class setting ()
  ((id :db-kind :key :type integer)
   (key :type string)
   (value :type string)))
