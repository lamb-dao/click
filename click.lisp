;;;; ===================================  set environment
                                        ; imports

(ql:quickload :unix-in-lisp) ;&&& load before clesh readtables?

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
(ql:quickload :cl-ppcre)
(ql:quickload :local-time)
(ql:quickload :1am)
(ql:quickload :lisp-unit2)
;;(ql:quickload :clog) ; very slow on swank/gray, breaking repl
(ql:quickload :verbose)
(ql:quickload :iterate)
(ql:quickload :cl-schedule)
(ql:quickload :mito)
(ql:quickload :access)
(ql:quickload :fset)
(ql:quickload :misc-extensions) ;for gmap helper for fset
(ql:quickload :listopia)
(ql:quickload :jzon)
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
                                        ; &&& rename package and or function

  (:export
   :pwd
   :ls
   :cd
   :cat
   :grep
   :which
   :echo
   :find-dir
   :*default-pathname-initialized*
   :*default-pathname-starts*
   ))

                                        ; enter package
(in-package :click) ; Also enter this in the REPL!

;;;; ==================================== initializations

(defun on-start ()
  "&&& everything that should happen every startup"
  (quicklisp:update-all-dists)
  &&& setup and export and doc
  ;; locations complementary to  *default-pathname-defaults*
  (defvar *default-pathname-start* (uiop:getcwd)
    "Set to the location from which invoked, unchangable")
  (defparameter *default-pathname-target* (uiop:getcwd)
    "Set to the location from which invoked, changable to set the project root")
  &&& start cl-schedule
  )
(on-start)

(defun on-target ()
  "&&& everything that should happen once the user has navigated to target location"

  (defparameter *default-pathname-target* (uiop:getcwd)
    "Set to the location from which invoked, changable to set the project root")
  )

(defun begin-events ()
  "&&& checks for ~/click if E then check for events.lisp")

(defun setup-unix-in-lisp ()
  "&&& unix-in-lisp check and install on init, no user interaction"
  ;; &&& if not there clone it
  ;; git clone https://github.com/PuellaeMagicae/unix-in-lisp ~/quicklisp/local-projects/unix-in-lisp/
  ;; &&& if there update it

  ;; known: dists are updated by on-start
  ;; call for deps
  (ql:quickload "unix-in-lisp")
  ;; finally invoke
  (require 'unix-in-slime "~/quicklisp/local-projects/unix-in-lisp/unix-in-slime")
  )
(setup-unix-in-lisp)

;;;; ==================================== file system utilities

(defun directory-depth (&optional (depth 1) (paths '(#P"/")))
  "Takes a list of paths as roots, and returns a list of paths of all dirs to depth below the roots"
  ;; (directory-depth 1 `(,*default-pathname-defaults*))
  ;; (directory-depth 1 `(,( user-homedir-pathname)))
  ;; (directory-depth 1 '("/home/user/"))
  ;; (directory-depth 0 '("/home/user/"))
  ;; &&& still returns root as string, apply pathname to list of paths
  ;; &&& better example calls

  (if (<= depth 0) ; base case 0 will go no deeper, returns paths back up to recursive call
      (remove-duplicates paths); return just paths
                                        ; nested list of subdirs
                                        ; flatten list
                                        ; drop dups
                                        ; assign
      (let ((subdirectories (remove-duplicates (alexandria:flatten (mapcar #'uiop:subdirectories paths))
                                               :test #'equal)))
                                        ; recursive call into subdirectories
                                        ; drop dups
                                        ; add paths of this call to recursive result
                                        ; drop dups
                                        ; return
        (remove-duplicates (append paths (remove-duplicates (directory-depth (1- depth) subdirectories)
                                         :test #'equal))))))

(defun find-dir (&key
                   (root '(#P"/"))
                   (depth 0)
                   (keep "*")
                   (drop nil drop-supplied-p)
                   (show nil)
                   (n-test 1 n-test-supplied-p))
  "Digs into directory tree, fuzzy filters directories, can be sequentially applied to its own output.

  arg : default : description
  root : '(#P\"/\") : list with a path or paths in #P or string form
  depth : 0 : an integer for retrieval depth, searches below roots, 0 goes no deeper to filter root list
  keep : all : space delimited words to separately(additive) fuzzy filter in
  drop : none : space delimited words to separately(additive) fuzzy filter out, applied after keep
  show : none : set true for reporting on processing, big ass reports if depth is heavy
  n-test : off : integer for required num of paths in output list, error and report if not "
  ;; (find-dir :keep "lib opt" :drop "etc lib gnu")
  ;; (find-dir :n-test 6 :keep "opt" :drop "gnu"  )
  ;; (find-dir :depth 0 :drop "chrome" :root (find-dir :n-test 6 :keep "opt" :drop "gnu"  ))
  ;; &&& better example calls
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
        (when show (print-report))
        (when n-test-supplied-p
          (unless (= n-test (length keeps))
            (print-report)
            (error "In find-dir incorrect number of directories~%expected: ~D found: ~D~%" n-test (length keeps))))
        ;;return
        final))))

;;;; ==================================== bash wrappers
(defun pwd ()
  "Print working directory"
  (uiop:getcwd))

(defun ls (&optional args)
  "List dir contents.
  TODO The current implementation is basic. It could be improved by adding support for various flags like -l (long format), -a (show hidden files)"
  (if (null args)
      ($cmd "ls")
      ($cmd (format nil "ls ~A" args))))

(defun cd (&optional path)
  "Change directory. Defaults to ~"
  (let ((target-path (cond
                       ((null path)(user-homedir-pathname))
                       ((string= path "..")(uiop:pathname-parent-directory-pathname (uiop:getcwd)))
                       ((string= path "~")(user-homedir-pathname))
                       (t path))))
    (uiop:chdir target-path)
    (setf *default-pathname-defaults* (pwd)) ;lock in
    (pwd)))

(defun grep (pattern &rest files-or-input)
  "Search for pattern in files or input stream.
TODO untested."
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

(defun cat (filename)
  "Display contents of a file"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line do (format t "~A%" line))))


(defun which (command)
  "Find path of an executable"
  ($cmd (format nil "which ~A" command)))

(defun echo (&rest args)
  "Print arguments to standard ouput"
  (format t "~{~A~%~}~%" args))

(defun mkdir (dirname)
  "Create a new directory in the current working directory"
  (let ((new-dir (merge-pathnames (pathname dirname) (pwd))))
    (ensure-directories-exist (format nil "~A/" new-dir)))) ; need trailing slash after dirname to create the final dir!

(defun rmdir (dirname)
  "Remove an empty directory if it exists in the current working directory"
  (let ((target-dir (merge-pathnames (pathname dirname) (uiop:getcwd))))
    (uiop:delete-empty-directory (format nil "~A/" target-dir))))

(defun rm (filename)
  "Remove a file in the current working directory"
  (let ((target-file (merge-pathnames (pathname filename) (pwd))))
    (if (probe-file target-file)
        (progn
          (delete-file target-file)
          (format t "File '~A' deleted successfully.~%" filename))
        (format t "File '~A' not found.~%" filename))))

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

(defun chmod (mode filename)
  "Change file mode bits"
  (let ((result ($cmd (format nil "chmod ~A ~A" mode filename))))
    (if (string= result "")
        (format t "Changed mode of '~A' to ~A~%" filename mode)
        (format t "~A" result))))

(defun chown (owner filename)
  "Change ownership of a file"
  (let ((result ($cmd (format nil "chown ~A ~A" owner filename))))
    (if (string= result "")
        (format t "Changed ownership of '~A' to ~A~%" filename owner)
        (format t "~A" result))))









(defun whoami ()
  "Print the current user name"
  (let ((result ($cmd "whoami")))
    (if (string= result "")
        (format t "Unable to determine current user.~%")
        (format t "~A" result))))

(defun uname (&optional option)
  "Print system information"
  (let ((command (if option
                     (format nil "uname ~A" option)
                     "uname")))
    (let ((result ($cmd command)))
      (if (string= result "")
          (format t "Unable to retrieve system information.~%")
          (format t "~A" result)))))

(defun date (&optional format)
  "Print the current date and time"
  (let ((command (if format
                     (format nil "date +\"~A\"" format)
                     "date")))
    (let ((result ($cmd command)))
      (if (string= result "")
          (format t "Unable to retrieve date and time.~%")
          (format t "~A" result)))))












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


;;;; =================================== to implement

(defun sort (filename &optional reverse)
  "Sort lines of text file"
  (let* ((lines (with-open-file (stream filename)
                  (loop for line = (read-line stream nil)
                        while line
                        collect line)))
         (sorted-lines (sort lines #'string<)))
    (if reverse
        (setf sorted-lines (nreverse sorted-lines)))
    (dolist (line sorted-lines)
      (format t "~A~%" line))))
(defun time (command)
  "Execute a command and print time taken"
  (let* ((start-time (get-internal-real-time))
         (result ($cmd command))
         (end-time (get-internal-real-time))
         (elapsed-time (/ (- end-time start-time) internal-time-units-per-second)))
    (format t "~A" result)
    (format t "~%Time elapsed: ~,3F seconds~%" elapsed-time)))

(defun find (path &optional (expression ""))
  "Find files in a directory hierarchy"
  (let ((result ($cmd (format nil "find ~A ~A" path expression))))
    (if (string= result "")
        (format t "No files found.~%")
        (format t "~A" result))))

(defun touch (filename)
  "Create a new empty file or update the timestamp of an existing file"
  (let ((target-file (merge-pathnames (pathname filename) (pwd))))
    (if (probe-file target-file)
        ;&&& (uiop:touch-file target-file)
        (with-open-file (stream target-file :direction :output :if-does-not-exist :create)
          (declare (ignore stream))))
    (format t "Touched file: ~A~%" filename)))
(defun p (str &key (:p nil ) (:t "A" ))
  "TODO fast and simple printing utility
takes a string like: The (quick) brown (fox)
substitutes the inline parens to a format statement of the type specified
evaluates the format statement
if :p is nil then the evaluated string is returned from the pf form. ie. it is the second format arg
if :t is set to X then the format operator is ~X
"(&&&))


(defun finder-logic (find &key (root #P"/") (test nil) (retype #F))
  "Using a set of string keys collects a set of file paths as strings"
  ;;&&& a finder macro that takes key args :root :find :test :retype
  ;; :root must be a single dir of type #P
  ;; :test Defaults to nil. When true test the call. Testing prints the matches but return nil. Using is silent returns the matches.
  ;; :type sets return type from {#F #P string}, defaults to #F

  ;; :find '(<set)
  ;; where <set> can be (and (not (or <arg> ... <arg>))) or other set operations in arbitrary composition
  ;; where <arg> can be from finder syntax

  ;; prepare find string
  (with-current-directory root (&&&))
  )

(defun help (type)
  "&&& display a message that makes help and system info discoverable
type lets you drill down to a specific help type
easter-egg (help burrito) prints the recipie
eg system-apropos, describe")







(exit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;build

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;scratch

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;reference
