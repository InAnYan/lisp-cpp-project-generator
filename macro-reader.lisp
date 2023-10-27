(load (merge-pathnames "files-generator.lisp" *load-truename*))

(defparameter *namespaces* nil)

(defmacro namespace (name &body forms)
  `(progn (push ,name *namespaces*)
          (progn ,@forms)
          (pop *namespaces*)))

(defmacro klass (name)
  `(create-class-files (make-klass :name ,name
                                   :namespaces (reverse *namespaces*))))
(defmacro header (name)
  `(create-simple-header-file (reverse *namespaces*) ,name))

(defmacro pair (name)
  `(create-pair-files (make-klass :name ,name
                                   :namespaces (reverse *namespaces*))))

(defmacro exception (name &rest fields)
  `(create-excpetion-files (make-exception :name ,name
                                           :namespaces (reverse *namespaces*)
                                           :fields (quote ,fields))))
