(defsystem :click
  :depends-on (:alexandria
               :pathname-utils
               :cl-fad
               :filesystem-utils
               :file-attributes
               :osicat
               :filepaths
               :ppath
               :file-finder
               :cmd
               :py4cl
               :quicksearch
               :mito
               :cl-ppcre
               :local-time
               :1am
               :lisp-unit2
               :verbose
               :iterate
               :clerk
               :access
               :fset
               :misc-extensions
               :listopia
               :com.inuoe.jzon
               :cl-csv
               :fuzzy-match
               :str)
  :serial t
  :components ((:file "click") ; a .lisp file
               (:static-file "README.org")))
