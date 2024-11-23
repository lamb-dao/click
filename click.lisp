;;;; ===================================  set environment
                                        ; imports

(ql:quickload :pathname-utils)
(ql:quickload :cl-fad)
(ql:quickload :filesystem-utils)
(ql:quickload :file-attributes)
(ql:quickload :osicat)
(ql:quickload :filepaths)
(ql:quickload :ppath)
(ql:quickload :file-finder)
(ql:quickload :cmd)
(ql:quickload :py4cl)
(ql:quickload :quicksearch)
(ql:quickload :str)
(ql:quickload :mito)
(ql:quickload :cl-ppcre)
(ql:quickload :local-time)
(ql:quickload :1am)
(ql:quickload :lisp-unit2)
(ql:quickload :verbose)
(ql:quickload :iterate)
(ql:quickload :clerk)
(ql:quickload :access)
(ql:quickload :fset)
(ql:quickload :misc-extensions) ;for gmap helper for fset
(ql:quickload :listopia)
(ql:quickload :com.inuoe.jzon)
(ql:quickload :cl-csv)
(ql:quickload :fuzzy-match) ; for find-dir filtering

;(ql:quickload :clesh)
;(use-package :named-readtables)
;(in-readtable clesh:syntax)

                                        ; package def
(defpackage :click
                                        ; whole package import
  (:use :cl :str :cmd :file-finder)
                                        ; shadowing, declares dominant function
  (:shadowing-import-from :cmd :current-directory)
                                        ; specific function import to this namespace
  (:import-from :uiop
   :subdirectories :directory-files :getcwd)
                                        ; rename package and or function
  (:local-nicknames (:jzon :com.inuoe.jzon))
                                        ; export functions and params to the click: name space
  (:export
   :pwd
   :ls
   :cd
   :cat
   :grep
   :which
   :echo
   :find-dir
   :on-start
   :on-project
   :*default-pathname-initialized*
   :*default-pathname-project*
   ))

                                        ; enter package
(in-package :click) ; Also enter this in the REPL!

;;;; ==================================== initialization

(defun on-start ()
  "&&& everything that should happen every startup"
  (quicklisp:update-all-dists :prompt nil)
  ;; &&& start clerk
  ;; &&& clerk jobs
  ;; locations complementary to  *default-pathname-defaults*
  (defvar *default-pathname-initialized* (uiop:getcwd)
    "Set to the location from which invoked, immutable")
  (defparameter *default-pathname-project* (uiop:getcwd)
    "Set to the location from which invoked, changable to set the project root"))
(on-start)

(defun on-project ()
  "&&& everything that should happen once the user has navigated to target location"
  ;; additional clerk jobs
  (defparameter *default-pathname-project* (uiop:getcwd)
    "Set to the location from which invoked, changable to set the project root"))

;;;; ==================================== file system utilities
(defun directory-depth (&optional (depth 1) (paths '(#P"/")))
  "Takes a list of paths or a path as search roots, and returns a deduplicated list of paths of all dirs to depth below the roots that exist"
;; (directory-depth 1 `(,*default-pathname-defaults*))
;; (directory-depth 1 `(,(user-homedir-pathname)))
;; (directory-depth 1 '("/home/user/" "barfoo"))
;; (directory-depth 1 '("/home/user/"))
;; (directory-depth 0 '("/home/user/"))
;; (directory-depth 0 '("/home/user"))
                                        ; make real path, check it exists
                                        ; drop dups
  (let ((good-paths (remove-duplicates (remove-if #'null (mapcar #'uiop:directory-exists-p paths)))))
   (if (<= depth 0) ; base case 0 will go no deeper, returns paths back up to recursive call
      good-paths; return just paths
                                        ; nested list of subdirs
                                        ; flatten list
                                        ; drop dups
                                        ; assign
      (let ((subdirectories (remove-duplicates (alexandria:flatten (mapcar #'uiop:subdirectories good-paths))
                                               :test #'equal)))
                                        ; recursive call into subdirectories
                                        ; drop dups
                                        ; add paths of this call to recursive result
                                        ; drop dups
                                        ; return
        (remove-duplicates (append good-paths (remove-duplicates (directory-depth (1- depth) subdirectories)
                                         :test #'equal)))))))

(defun find-dir (&key
                   (root '(#P"/"))
                   (depth 0)
                   (keep "*")
                   (drop nil drop-supplied-p)
                   (report nil)
                   (n-test 1 n-test-supplied-p))
  "takes a list of paths and fuzzy filters directories, if depth is set it descends into directory tree,can be sequentially applied to its own output.

  arg : default action : description
  root : '(#P\"/\") : a list with paths or a path in #P or string form
  depth : 0 : an integer for retrieval depth below roots, default 0 goes no deeper for filtering root list
  keep : all : space delimited words to separately(additive to set) fuzzy filter in
  drop : none : space delimited words to separately(additive to set) fuzzy filter out, applied after keep
  report : none : set true for reporting on processing, big ass reports if depth is large
  n-test : off : integer for required num of paths in output list, error and report if not "
  ;; (find-dir :keep "lib opt" :drop "etc lib gnu" :depth 2)
  ;; (find-dir :report t :keep "lib opt" :drop "etc lib gnu" :depth 2)
  ;; (find-dir :n-test 6 :depth 3 :keep "opt" :drop "gnu"  )
  ;; (find-dir :drop "chrome" :root (find-dir :depth 2 :keep "opt" :drop "gnu"))
  (flet ((find-sets (dirs str)
           (let ((sets '())
                 (set-list (str:split " " str)))
             (dolist (s set-list)
               (setf sets (append (fuzzy-match:fuzzy-match s dirs)
                                  sets)))
             sets)))
    (let* ((dirs (directory-depth depth root))
           (keeps (find-sets dirs keep))
           (drops (if drop-supplied-p
                      (find-sets keeps drop)
                      '()))
           (final (set-difference keeps drops)))
      (labels ((show-dirs (title search-string dirs)
                 (format t "~&~%~A ~A~%" title search-string)
                 (dolist (d dirs)
                   (format t "~A~%" d)))
               (print-report ()
                 (show-dirs "RECIEVED" "" root)
                 (show-dirs "RETRIEVED" "" dirs)
                 (show-dirs "KEEPS" keep keeps)
                 (show-dirs "DROPS" drop drops)
                 (show-dirs "FINAL" "" final)))
        (when report (print-report))
        (when n-test-supplied-p
          (unless (= n-test (length final))
            (print-report)
            (error "In find-dir incorrect number of directories~%expected: ~D found: ~D~%" n-test (length keeps))))
        ;;return
        final))))

;;;; ==================================== click utilities

(defun help (type)
  "TODO display a message that makes help and system info discoverable
type lets you drill down to a specific help type
eg system-apropos, describe"
  (print "not implemented"))

(defun p (str &key (:p nil ) (:t "A" ))
  "TODO fast and simple printing utility
takes a string like: The (quick) brown (fox)
substitutes the inline parens to a format statement of the type specified
evaluates the format statement
if :p is nil then the evaluated string is returned from the pf form. ie. it is the second format arg
if :t is set to X then the format operator is ~X
"(&&&))

;; Commands are listed in priority order. The headings are moved down as work is completed

;;;; ==================================== bash portable complete cannonical
;; The command is not a wrapper, the full functionality is implemented in  portable CL

;;;; ==================================== bash, docstring is educative
;; The all the shell like commands should move the user toward the language.
;; The docstring educates the user on cannonical CL approach. May still be wrapper.

(defun ls (&optional argstring)
  "List contents of pwd. returns dirs then files as CL pathnames. If an arg string is provided those will be passed to unix ls.
Cannonical
dirs: (uiop:subdirectories (uiop:getcwd))
files: (uiop:directory-files (uiop:getcwd))"
  ;; (ls) => pathnames
  ;; (ls "-al") =>
  (if (null argstring)
      (append
       (uiop:subdirectories (uiop:getcwd))
       (uiop:directory-files (uiop:getcwd)))
      ($cmd (format nil "ls ~A" argstring))))

(defun pwd ()
  "Print working directory. (uiop:getcwd)"
  (uiop:getcwd))

;;;; ==================================== bash tested
;; The command basically works as it should, if any functionality is missing it should be noted in docstring.

(defun cd (&optional path)
  "Change directory. Defaults to ~"
  (let ((target-path (cond
                       ((null path)(user-homedir-pathname))
                       ((string= path "..")(uiop:pathname-parent-directory-pathname (uiop:getcwd)))
                       ((string= path "~")(user-homedir-pathname))
                       (t path))))
    ;; &&& could test if target-path exists
    (uiop:chdir target-path)
    (setf *default-pathname-defaults* (pwd)) ;lock in
    (pwd)))

(defun which (command)
  "Find path of an executable"
  ($cmd (format nil "which ~A" command)))

(defun cat (filename)
  "Display contents of a file"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line do (format t "~A~%" line))))

(defun head (filename &optional (n 10))
  "Display the first N lines of a file (default 10)"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          for i from 1 to n
          while line
          do (format t "~A~%" line))))

(defun tail (filename &optional (n 10))
  "Display the last N lines of a file (default 10)"
  (with-open-file (stream filename)
    (let ((lines (loop for line = (read-line stream nil)
                       while line
                       collect line)))
      (loop for line in (last lines n)
            do (format t "~A~%" line)))))

;;;; ==================================== end of evaluated code
(exit)
;; Commands are listed in priority order. The headings are moved down as work is completed



;;;; =================================== active construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; build

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; scratch

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; reference


;;;; ==================================== bash rough implementation
;; The command is implemented to provide simple function. Use at your own risk and test as you go.
;; It may be a wrapper, it might even be slopfully slop

(defun rm (filename)
  "Remove a file in the current working directory"
  (let ((target-file (merge-pathnames (pathname filename) (pwd))))
    (if (probe-file target-file)
        (progn
          (delete-file target-file)
          (format t "File '~A' deleted successfully.~%" filename))
        (format t "File '~A' not found.~%" filename))))

(defun mv (source destination)
  "Move a file from source to destination
TODO Not recursive Only Files"
  (let ((src (merge-pathnames (pathname source) (pwd)))
        (dest (merge-pathnames (pathname destination) (pwd))))
    (if (probe-file src)
        (progn
          (rename-file src dest)
          (format t "File '~A' moved to '~A' successfully.~%" source destination))
        (format t "Source file '~A' not found.~%" source))))

(defun cp (source destination)
  "Copy a file from source to destination.
TODO Not recursive. Only files"
  (let ((src (merge-pathnames (pathname source) (pwd)))
        (dest (merge-pathnames (pathname destination) (pwd))))
    (if (probe-file src)
        (progn
          (uiop:copy-file src dest)
          (format t "File '~A' copied to '~A' successfully.~%" source destination))
        (format t "Source file '~A' not found.~%" source))))

(defun touch (filename)
  "Create a new empty file or update the timestamp of an existing file"
  (let ((target-file (merge-pathnames (pathname filename) (pwd))))
    (if (probe-file target-file)
                                        ;&&& (uiop:touch-file target-file)
        (with-open-file (stream target-file :direction :output :if-does-not-exist :create)
          (declare (ignore stream))))
    (format t "Touched file: ~A~%" filename)))

(defun rmdir (dirname)
  "Remove an empty directory if it exists in the current working directory"
  (let ((target-dir (merge-pathnames (pathname dirname) (uiop:getcwd))))
    (uiop:delete-empty-directory (format nil "~A/" target-dir))))

(defun mkdir (dirname)
  "Create a new directory in the current working directory"
  (let ((new-dir (merge-pathnames (pathname dirname) (pwd))))
    (ensure-directories-exist (format nil "~A/" new-dir)))) ; need trailing slash after dirname to create the final dir!

(defun echo (&rest args)
  "Print arguments to standard ouput"
  (format t "~{~A~%~}~%" args))

(defun chown (owner filename)
  "Change ownership of a file"
  ;; TODO use file-utilities for this
  (let ((result ($cmd (format nil "chown ~A ~A" owner filename))))
    (if (string= result "")
        (format t "Changed ownership of '~A' to ~A~%" filename owner)
        (format t "~A" result))))

(defun chmod (mode filename)
  "Change file mode bits"
  ;; TODO use file-utilities for this
  (let ((result ($cmd (format nil "chmod ~A ~A" mode filename))))
    (if (string= result "")
        (format t "Changed mode of '~A' to ~A~%" filename mode)
        (format t "~A" result))))

(defun grep (pattern &rest files-or-input)
  "Search for pattern in files or input stream."
  ;; TODO untested slop.
  (labels ((process-line (line source)
             (when (search pattern line)
               (format t "~A: ~A~%" source line)))
           (process-file (filename)
             (with-open-file (stream filename)
               (loop for line = (read-line stream nil)
                     while line
                     do (process-line line filename))))
           (process-stream (stream)
             (loop for line = (read-line stream nil)
                   while line
                   do (process-line line "stdin"))))
    (if files-or-input
        (dolist (file-or-input files-or-input)
          (if (streamp file-or-input)
              (process-stream file-or-input)
              (process-file file-or-input)))
        (process-stream *standard-input*))))

(defun wc (&optional filename)
  "Count lines, words, and characters in a file or from standard input"
  (let ((lines 0)
        (words 0)
        (chars 0)
        (in-word nil))
    (with-open-file (stream (or filename *standard-input*)
                            :if-does-not-exist nil)
      (when stream
        (loop for char = (read-char stream nil :eof)
              until (eq char :eof)
              do (incf chars)
                 (case char
                   (#\Newline (incf lines))
                   ((#\Space #\Tab #\Newline)
                    (when in-word
                      (incf words)
                      (setf in-word nil)))
                   (otherwise
                    (setf in-word t)))
              finally (when
                          in-word (incf words)))))
    (format t "~5d ~5d ~5d~@[ ~A~]~%" lines words chars filename)))

(defun uniq (filename &optional count)
  "Filter adjacent matching lines from input"
  (with-open-file (stream filename)
    (let ((prev-line nil)
          (count-num 1))
      (loop for line = (read-line stream nil)
            while line
            do (if (string= line prev-line)
                   (incf count-num)
                   (progn
                     (when prev-line
                       (if count
                           (format t "~A ~A~%" count-num prev-line)
                           (format t "~A~%" prev-line)))
                     (setf prev-line line)
                     (setf count-num 1)))
            finally (when prev-line
                      (if count
                          (format t "~A ~A~%" count-num prev-line)
                          (format t "~A~%" prev-line)))))))

;;;; =================================== bash planned

whoami
date
uname

;;;; =================================== nushell planned

oh maybe all of it?
Just the raddest bits of nushell, the danger of even considering implementing nushell in CL is like sirens calling me.
