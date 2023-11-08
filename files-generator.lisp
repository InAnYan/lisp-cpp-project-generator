(defstruct klass
  namespaces
  name)

(defstruct (exception (:include klass))
  fields)

(defun make-klass-header-file-path (cls)
  (concatenate 'string 
               (make-base-path cls)
               ".hpp"))

(defun make-klass-source-file-path (cls)
  (concatenate 'string 
               (make-base-path cls)
               ".cpp"))

(defun make-base-path (cls)
  (concatenate 'string (make-folder-path (klass-namespaces cls))
               (klass-name cls)))

(defun make-folder-path (namespaces)
  (concatenate 'string (join "/" namespaces)
               "/"))

(defun make-pair-header-file (cls)
  (make-header-guard cls
                     (make-namespaces (reverse (klass-namespaces cls))
                                      (list))))

(defun make-pair-source-file (cls)
  (append (list (list "#include" (concatenate 'string "\"" (klass-name cls) ".hpp" "\""))
                ())
          (make-namespaces (reverse (klass-namespaces cls))
                           (list ()))))

(defun make-klass-header-file (cls)
  (make-header-guard cls
                     (make-namespaces (reverse (klass-namespaces cls))
                                      (make-class-content cls))))

(defun make-klass-source-file (cls)
  (append (list (list "#include" (concatenate 'string "\"" (klass-name cls) ".hpp" "\""))
                ())
          (make-namespaces (reverse (klass-namespaces cls))
                           (make-class-implementation cls))))

(defun make-exception-header-file (ex)
  (make-header-guard ex
                     (append (list (list "#include" "<exception>")
                                   (list "#include" "<iostream>")
                                   ())
                             (make-namespaces (reverse (exception-namespaces ex))
                                              (append (make-exception-content ex)
                                                      (list ())
                                                      (make-exception-ostream-def ex))))))

(defun make-exception-ostream-def (ex)
  (list (list (concatenate 'string (make-exception-ostream-prototype-str ex) ";"))))

(defun make-exception-ostream-prototype-str (ex)
  (concatenate 'string "std::ostream& operator<<(std::ostream& out, " (make-type-cons-ref-str (exception-name ex)) " e)"))

(defun make-type-cons-ref-str (name)
  (concatenate 'string "const " name "&"))

(defun make-exception-source-file (ex)
  (append (list (list "#include" (concatenate 'string "\"" (exception-name ex) ".hpp" "\""))
                ())
          (make-namespaces (reverse (exception-namespaces ex))
                           (append (make-exception-implementation ex)
                                   (list ())
                                   (make-exception-ostream-implementation ex)))))

(defun make-exception-ostream-implementation (ex)
  (list (list (make-exception-ostream-prototype-str ex))
        (list "{")
        (list "return" ";")
        (list "}")))

(defun make-exception-implementation (ex)
  (append (make-exception-constructor-implementation ex)
          (list ())
          (make-exception-what-implementation ex)
          (if (exception-fields ex)
              (append (list ())
                      (make-exception-methods-implementations ex)))))

(defun make-exception-what-implementation (ex)
  (list (make-exception-what-implementation-prototype ex)
        (list "{")
        (list "return" ";")
        (list "}")))

(defun make-exception-what-implementation-prototype (ex)
  (list "const" "char*" (concatenate 'string
                                     (exception-name ex)
                                     "::what()")
        "const"
        "throw()"))

(defun make-exception-constructor-implementation (ex)
  (list (make-exception-constructor-implementation-prototype ex)
        (make-exception-constructor-preinit ex)
        (list "{")
        (list ())
        (list "}")))

(defun make-exception-constructor-implementation-prototype (ex)
  (list (concatenate 'string
                     (exception-name ex)
                     "::"
                     (exception-name ex)
                     "("
                     (make-exception-constructor-parameters-str ex)
                     ")")))

(defun make-exception-constructor-preinit (ex)
  (list (concatenate 'string
                     "    : "
                     (join ", " (make-exception-constructor-preinit-clauses-list ex)))))

(defun make-exception-constructor-preinit-clauses-list (ex)
  (mapcar #'(lambda (field)
              (concatenate 'string
                           (second field)
                           "("
                           (second field)
                           ")"))
          (exception-fields ex)))

(defun make-exception-methods-implementations (ex)
  (join (list ())
        (mapcar #'(lambda (field)
                    (make-exception-method-implementation field (exception-name ex)))
                (exception-fields ex))
        #'append))

(defun make-exception-method-implementation (field name)
  (list (make-exception-method-implementation-prototype field name)
        (list "{")
        (list "return" (concatenate 'string (second field) ";"))
        (list "}")))

(defun make-exception-method-implementation-prototype (field name)
  (list (concatenate 'string
                     (first field)
                     " "
                     name
                     "::"
                     "Get"
                     (string-capitalize (second field))
                     "() const")))

(defun make-exception-content (ex)
  (append (list (list "class" (exception-name ex) ":" "public" "std::exception")
                (list "{")
                (list "public:")
                (list (make-exception-constructor-decl-str ex))
                ())
          (make-exception-methods-decl ex)
          (list ())
          (list (list "virtual const char* what() const throw();"))
          (list ())
          (list (list "private:"))
          (make-exception-fields ex)
          (list (list "};" "//" "class" (exception-name ex)))))

(defun make-exception-fields (ex)
  (mapcar #'(lambda (field)
              (list (concatenate 'string field ";")))
          (make-exception-fields-list ex)))

(defun make-exception-methods-decl (ex)
  (mapcar #'make-exception-method-decl (exception-fields ex)))

(defun make-exception-method-decl (field)
  (list (concatenate 'string
                     (first field)
                     " "
                     (concatenate 'string "Get" (string-capitalize (second field)))
                     "() const;")))

(defun make-exception-constructor-decl-str (ex)
  (concatenate 'string (make-exception-constructor-base-str ex) ";"))

(defun make-exception-constructor-base-str (ex)
  (concatenate 'string
               (exception-name ex)
               "("
               (make-exception-constructor-parameters-str ex)
               ")"))

(defun make-exception-constructor-parameters-str (ex)
  (join ", " (make-exception-fields-list ex)))

(defun make-exception-fields-list (ex)
  (mapcar #'(lambda (field)
              (join " " field))
          (exception-fields ex)))

(defun make-class-implementation (cls)
  (list (list (concatenate 'string (klass-name cls)
                           "::"
                           (klass-name cls)
                           "()"))
        (list "{")
        ()
        (list "}")))

(defun make-header-guard (cls text)
  (let ((guard (make-header-guard-text cls)))
    (append
     (list (list "#ifndef" guard)
           (list "#define" guard)
           ())
     text
     (list
      ()
      (list "#endif //" guard)))))

(defun make-header-guard-text (cls)
  (join "_" 
        (mapcar #'string-upcase 
                (append (klass-namespaces cls)
                        (list (klass-name cls) "hpp")))))


(defun make-namespaces (namespaces text)
  (reduce #'make-namespace namespaces :initial-value text))

(defun make-namespace (text namespace)
  (append
   (list (list "namespace" namespace)
         (list "{"))
   text
   (list (list "}"))))

(defun make-class-content (cls)
  (list (list "class" (klass-name cls))
        (list "{")
        (list "public:")
        (list (concatenate 'string (klass-name cls) "();"))
        ()
        (list "private:")
        ()
        (list "};" "//" "class" (klass-name cls))))

(defun join (delim strs &optional (fn #'(lambda (a b c) (concatenate 'string a b c))))
  (if strs
      (if (= (length strs) 1)
          (first strs)
          (funcall fn
                   (first strs)
                   delim
                   (join delim (rest strs))))
      ""))

(defparameter *new-line-str* (format nil "~%")) ; :)

(defun lines-to-string (lines)
  (indent-lines (mapcar #'(lambda (line)
                            (concatenate 'string (join " " line) *new-line-str*))
                        lines)))

(defun indent-lines (lines &optional (level 0))
  (if lines
      (concatenate 'string 
                   (make-indent (+ level (determine-pre-indent-change (first lines))))
                   (first lines) 
                   (indent-lines (rest lines) 
                                 (+ level (determine-post-indent-change (first lines)))))
      ""))

(defparameter *indent* "    ")

(defun make-indent (level)
  (if (= level 0)
      ""
      (concatenate 'string *indent* (make-indent (- level 1)))))

(defun determine-post-indent-change (str)
  (cond
    ((eql (char str 0) #\{) 1)
    ((eql (char str 0) #\}) -1)
    (t 0)))

(defun determine-pre-indent-change (str)
  (cond
    ((eql (char str 0) #\}) -1)
    ((or (equal str (format nil "private:~%"))
         (equal str (format nil "public:~%")))
     -1)
    (t 0)))

;; (defparameter *test-cls* (make-klass :name "Token" :namespaces '("Lox" "Compiler")))
;; (format t "~a" (lines-to-string (make-klass-header-file *test-cls*)))
;; (format t "~a" (lines-to-string (make-klass-source-file *test-cls*)))

(defun make-simple-header-file (namespaces name)
  (make-header-guard (make-klass :namespaces namespaces
                                 :name name)
                     (make-namespaces (reverse namespaces) nil)))

(defun create-class-files (cls)
  (ensure-directories-exist (make-folder-path (klass-namespaces cls)))
  (with-open-file (header-file (make-klass-header-file-path cls)
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists nil)
    (format header-file "~a" (lines-to-string (make-klass-header-file cls))))
  (with-open-file (source-file (make-klass-source-file-path cls)
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists nil)
    (format source-file "~a" (lines-to-string (make-klass-source-file cls)))))

(defun create-simple-header-file (namespaces name)
  (ensure-directories-exist (make-folder-path namespaces))
  (with-open-file (header-file (make-klass-header-file-path (make-klass :name name
                                                                        :namespaces namespaces))
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists nil)
    (format header-file "~a" (lines-to-string (make-simple-header-file namespaces name)))))

(defun create-excpetion-files (ex)
  (ensure-directories-exist (make-folder-path (exception-namespaces ex)))
  (with-open-file (header-file (make-klass-header-file-path ex)
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists nil)
    (format header-file "~a" (lines-to-string (make-exception-header-file ex))))
  (with-open-file (source-file (make-klass-source-file-path ex)
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists nil)
    (format source-file "~a" (lines-to-string (make-exception-source-file ex)))))


(defun create-pair-files (cls)
  (ensure-directories-exist (make-folder-path (klass-namespaces cls)))
  (with-open-file (header-file (make-klass-header-file-path cls)
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists nil)
    (format header-file "~a" (lines-to-string (make-pair-header-file cls))))
  (with-open-file (source-file (make-klass-source-file-path cls)
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists nil)
    (format source-file "~a" (lines-to-string (make-pair-source-file cls)))))
