;;;; ===================================  set environment
                                        ; imports
(ql:quickload :cmd)
(ql:quickload :lisp-stat)
(ql:quickload :lparallel)
(ql:quickload :py4cl)
(ql:quickload :str)
(ql:quickload :file-finder)

                                        ; package def
(defpackage :click
                                        ; whole package import
  (:use :cl :cmd :str :file-finder)
                                        ; specific function import to this namespace, the rest are behind the :package prefix
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
   :*start-directory*
   ))

                                        ; enter package
(in-package :click) ; Also enter this in the REPL!

;;;; ==================================== location
                                        ; &&& test from external usage
(defparameter *start-directory* (uiop:getcwd))

;;;; ==================================== bash wrappers
(defun pwd ()
  "Print working directory"
  (uiop:getcwd))

(defun ls (&optional args)
  "List dir contents"
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

(defun cat (filename)
  "Display contents of a file"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line do (format t "~A%" line))))

(defun grep (pattern filename)
  "Search for pattern in file"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          when (search pattern line)
            do (format t  "~A%" line))))

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

;;;; ==================================== nushell wrappers

;;;; =================================== to implement

(defun p (str &key (:p nil ) (:t "A" ))
  "&&& simple printing utility
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
:root defaults to root
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
