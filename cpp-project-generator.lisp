(load (merge-pathnames "files-generator.lisp" *load-truename*))
(load (merge-pathnames "macro-reader.lisp" *load-truename*))

(load (second *posix-argv*))
