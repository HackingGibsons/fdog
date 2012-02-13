(in-package :http-dog)

(defcategory router)
(defmacro with-dispatch-on (route binding matched-form &body rules)
  (log-for (router trace) "Dispatching on ~A" route)
  (let (exact regex errors)
    (dolist (rule rules)
      (log-for (router trace) "Considering: ~A" rule)
      (destructuring-bind (type &rest options) rule
        (case type
          (:exact (log-for (router trace) "Exact route: ~A" options)
                  (push options exact))
          (:regex (log-for (router trace) "Regex route: ~A" options)
                  (push options regex))
          (otherwise (log-for (router trace) "Error: ~A" rule)
                     (push rule errors)))))
    (let ((g!route (gensym "route"))
          (g!exact (gensym "exact"))
          (g!regex (gensym "regex"))
          (g!error (gensym "error"))
          (g!match (gensym "match")))
      (log-for (router trace) "Exacts: ~A" exact)

      `(let* ((,g!route ,route) (,g!exact ',exact) (,g!regex ',regex) (,g!error ',errors)
              (,g!match (or (dolist (e-route ,g!exact)
                              (log-for (router dribble) "=> Current Exact test: ~A" e-route)
                              (destructuring-bind (path &rest options) e-route
                                (log-for (router dribble) "Checking exact route: ~A" path)
                                (when (string= path ,g!route)
                                  (log-for (router dribble) "Matched exact route: ~A => ~A" path options)
                                  (return options))))

                            (dolist (r-route ,g!regex)
                              (log-for (router dribble) "=> Current Regex test: ~A" r-route)
                              (destructuring-bind (path &rest options) r-route
                                (log-for (router dribble) "Checking regex route: ~A" path)
                                (when (ppcre:scan path ,g!route)
                                  (log-for (router dribble) "Matched regex route: ~A => ~A" path options)
                                  (return options))))

                            (cdr (or (find :default ,g!error :key #'car)
                                     (find :404 ,g!error :key #'car))))))
         (log-for (router dribble) "Matched route: ~A" ,g!match)
         (when ,g!match
           (let ((,binding (eval (getf ,g!match :responder))))
             ,matched-form))))))
