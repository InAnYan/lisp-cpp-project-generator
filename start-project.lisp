(load (merge-pathnames "files-generator.lisp" *load-truename*))

(defun main (project-name)
  (let* ((dirs (add-prefix project-name 
                           (list "bin" "src" "obj" "scripts")))
         (files (add-prefix project-name
                            (list "Makefile" ".gitignore"))))
    (create-directory project-name)
    (mapc #'create-directory dirs)
  ;  (mapc #'create-empty-file files)
  ;  (mapc #'create-empty-file (add-postfix ".gitkeep" dirs))))
    ))

(defun create-directory (path)
  (ensure-directories-exist (make-path-from-list path :directory)))

(defun create-empty-file (path)
  (with-open-file (file (make-path-from-list path :file)
                        :direction :output
                        :if-does-not-exist :create)
    (format file "")))

(defun add-prefix (prefix obj)
  (if (listp obj)
      (mapcar #'(lambda (path)
                  (cons prefix (if (listp path)
                                   path
                                   (list path))))
              obj)
      (cons prefix (list obj))))

(defun add-postfix (postfix obj)
  (if (listp obj)
      (mapcar #'(lambda (path)
                  (append path (list postfix)))
              obj)
      (append (list obj) (list postfix))))

(defun make-path-from-list (obj &optional (path-type :file))
  (let ((path (if (listp obj)
                  (join "/" obj)
                  obj)))
    (if (eq path-type :directory)
        (concatenate 'string path "/")
        path)))

(main (second *posix-argv*))
