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
(ql:quickload :cl-str) ; wrong string? &&&
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

(ql:quickload :clesh)
(use-package :named-readtables)
(in-readtable clesh:syntax)

                                        ; package def
(defpackage :click
                                        ; whole package import
  (:use :cl :str :cmd :file-finder)
                                        ; shadowing, declares dominant function
  (:shadowing-import-from :cmd :current-directory)
                                        ; specific function import to this namespace
  (:import-from :uiop
   :subdirectories :directory-files :getcwd)

  (:export
   :pwd
   :ls
   :cd
   :cat
   :grep
   :which
   :echo
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

(defun find-dir (&key (root "/") &rest )
  "Finds a single directory
takes and applies a sequence of (directory strings)
elegant error cases:
if none are found to match reports the last arg with a find
if multiple dirs are found reports the dirs at point
:root defaults to from root
")

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
eg system-apropos, describe, burrito etc")




I removed all hu.dwim* packages, updated all dists,  then began install of the package of interest.
We see it fetches the hu.dwim* and errors on the last.


To load "unix-in-lisp":
  Load 1 ASDF system:
    unix-in-lisp
; Loading "unix-in-lisp"
...........To load "hu.dwim.walker":
  Load 8 ASDF systems:
    alexandria anaphora asdf closer-mop contextl
    hu.dwim.asdf iterate metabang-bind
  Install 7 Quicklisp releases:
    hu.dwim.common hu.dwim.common-lisp hu.dwim.def
    hu.dwim.defclass-star hu.dwim.syntax-sugar hu.dwim.util
    hu.dwim.walker
; Fetching #<URL "http://beta.quicklisp.org/archive/hu.dwim.syntax-sugar/2023-02-14/hu.dwim.syntax-sugar-stable-git.tgz">
; 18.56KB
==================================================
19,007 bytes in 0.01 seconds (1546.79KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/archive/hu.dwim.common-lisp/2021-02-28/hu.dwim.common-lisp-stable-git.tgz">
; 2.05KB
==================================================
2,104 bytes in 0.00 seconds (2054.69KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/archive/hu.dwim.common/2015-07-09/hu.dwim.common-20150709-darcs.tgz">
; 3.01KB
==================================================
3,083 bytes in 0.00 seconds (3010.74KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/archive/hu.dwim.util/2021-12-30/hu.dwim.util-stable-git.tgz">
; 49.65KB
==================================================
50,837 bytes in 0.04 seconds (1272.93KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/archive/hu.dwim.defclass-star/2021-12-30/hu.dwim.defclass-star-stable-git.tgz">
; 8.90KB
==================================================
9,114 bytes in 0.00 seconds (0.00KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/archive/hu.dwim.def/2021-12-30/hu.dwim.def-stable-git.tgz">
; 19.65KB
==================================================
20,120 bytes in 0.01 seconds (2183.16KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/archive/hu.dwim.walker/2022-07-07/hu.dwim.walker-stable-git.tgz">
; 37.45KB
==================================================
38,345 bytes in 0.04 seconds (1069.89KB/sec)
; Loading "hu.dwim.walker"
..................................................
[package hu.dwim.def].............................
[package hu.dwim.defclass-star]...................
[package hu.dwim.common-lisp].....................
[package hu.dwim.common]..........................
[package hu.dwim.syntax-sugar]....................
[package hu.dwim.util]............................
[package hu.dwim.util/error-reports].

; file: /home/user/quicklisp/dists/quicklisp/software/hu.dwim.util-stable-git/source/miscellaneous.lisp
; in: DEF #'IOE
;     (METABANG.BIND:BIND #*
;       ((:SBCL ((*EVALUATOR-MODE* :INTERPRET))))
;       (EVAL HU.DWIM.UTIL::FORM))
;
; caught ERROR:
;   during macroexpansion of
;   (BIND #*
;     (#)
;     ...).
;   Use *BREAK-ON-SIGNALS* to intercept.
;
;    The value
;      #*
;    is not of type
;      LIST

;
; caught ERROR:
;   READ error during COMPILE-FILE:
;
;     no dispatch function defined for #\t
;
;       Line: 33, Column: 34, File-Position: 1047
;
;       Stream: #<SB-INT:FORM-TRACKING-STREAM for "file /home/user/quicklisp/dists/quicklisp/software/hu.dwim.util-stable-git/source/miscellaneous.lisp" {100BB290A3}>
;
; compilation unit aborted
;   caught 2 fatal ERROR conditions
;   caught 2 ERROR conditions
; Evaluation aborted on #<UIOP/LISP-BUILD:COMPILE-FILE-ERROR {100BBFC3C3}>.



The function found around line 33 is this. In fact line 33 has the prepended #* notation I do not understand
possibly a binary reader macro

(def (function e) quit (status-code)
  ;; (log.info "Quiting production image with status-code ~A" status-code)
  #*((:sbcl (sb-ext:exit :abort #t :code status-code))
     (:ccl (ccl:quit status-code))
     (t (not-yet-implemented/crucial-api 'quit))))
