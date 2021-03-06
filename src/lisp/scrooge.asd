;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage :scrooge-asdf
  (:use :cl :cl-user :asdf)
  (:export
   :*scrooge-version*
   :status
   :versions))

(in-package :scrooge-asdf)


(defvar *scrooge-version* "1.2.1"
  "A string denoting the current version of Scrooge.")

(export '*scrooge-version*)

(defsystem :scrooge
  :version #.*scrooge-version*
  :serial t
  ;;
  :depends-on ("puri"
               "ironclad"
               (:version "json" "1.0.0")
               (:version "lisputils" "1.1.0")
               (:version "veil" "1.1.0")
               (:version "bricks" "2.0.0")
               (:version "mortar" "1.1.0"))
  ;;
  :components ((:file "package")
               (:file "utils")
               (:file "tables")
               (:file "init")
               (:file "errors")
               (:file "globals")
               (:file "checks")
               (:file "family")
               (:file "ranks")
               (:file "calc")
               (:file "widgets")
               (:file "elements")
               ;; -- Here come the pages! --
               ;; auth
               (:file "auth")
               ;; section roots
               (:file "config/config")
               (:file "main/main")
               (:file "advanced/advanced")
               ;; config
               (:file "config/bank")
               (:file "config/tof")
               (:file "config/city")
               (:file "config/account")
               (:file "config/role")
               (:file "config/temtx")
               (:file "config/cheque-stran")
               ;; company & project
               (:file "main/company")
               (:file "main/project")
               ;; tx
               (:file "advanced/tx")
               (:file "advanced/account")
               ;; abstractions on txs
               (:file "main/cash")
               (:file "main/invoice")
               (:file "main/cheque")
               (:file "main/libtx")
               ;; company & project extras
               (:file "main/contact")
               (:file "main/bill")
               (:file "main/company-tx")
               (:file "main/company-cheque")
               ;; api
               (:file "root")))

(defsystem :scrooge-test
  :depends-on (:scrooge :hu.dwim.stefil :drakma)
  :components ((:module "test"
                :serial t
                :components ((:file "parameters")
                             (:file "tests")
                             (:file "test-utils")))))


(load-system :trivial-shell)
(use-package :trivial-shell)
(use-package :asdf)

;;; temporary code
(when (and (= (parse-integer (subseq (asdf-version) 0 1))
              2)
           (<= (parse-integer (subseq (asdf-version) 2))
               26))
  (defun component-sibling-dependencies (system)
    (cdr (assoc 'load-op
                (component-depends-on 'load-op system)))))

(defun versions (&optional (system-name "scrooge"))
  (let* ((systems (mapcar #'second
                          (remove-if #'atom
                                     (component-sibling-dependencies (find-system system-name)))))
         (max-length (reduce #'max (mapcar #'length systems))))
    (mapcar (lambda (name)
              (let ((ver (component-version (find-system name))))
                (format t "~A:~vT~A~&" name (+ max-length 2) ver)
                (list :version name ver)))
            systems)))

(defun status ()
  (mapc (lambda (sys)
          (let ((path (component-pathname (find-system sys))))
            (princ "SYSTEM: ")
            (princ sys)
            (terpri)
            (princ (shell-command (format nil "cd ~A; git status" path)))
            (terpri)))
        (list "scrooge" "json" "lisputils" "veil" "mortar" "bricks"))
  (values))
