;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage :scrooge-asdf
  (:use :cl :asdf)
  (:export
   :*scrooge-version*
   :status
   :versions))

(in-package :scrooge-asdf)


(defvar *scrooge-version* "1.0.3"
  "A string denoting the current version of Scrooge.")

(export '*scrooge-version*)


(defsystem :scrooge
  :version #.*scrooge-version*
  :serial t
  ;;
  :depends-on ("ironclad"
               (:version "json" "1.0.0")
               (:version "lisputils" "1.0.0")
               (:version "mortar" "1.0.2"))
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
               (:file "elements")
               (:file "widgets")
               ;; -- Here come the pages! --
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
               ;; root
               (:file "root")))

(defsystem :scrooge-test
  :depends-on (:scrooge :hu.dwim.stefil :drakma)
  :components ((:module "test"
                :serial t
                :components ((:file "tests")
                             (:file "test-utils")))))

(load-system :trivial-shell)

(use-package :trivial-shell)

(defun versions ()
  (mapcar (lambda (sys)
            (let ((ver (component-version (asdf:find-system sys))))
              (format t "~A: ~A~&" sys ver)
              (list :version sys ver)))
          (list "json" "lisputils" "veil" "mortar" "bricks")))

(defun status ()
  (mapc (lambda (sys)
            (let ((path (component-pathname (asdf:find-system sys))))
              (princ "SYSTEM: ")
              (princ sys)
              (terpri)
              (princ (shell-command (format nil "cd ~A; git status" path)))
              (terpri)))
          (list "json" "lisputils" "veil" "mortar" "bricks"))
  (values))


(defun versioned-compile-dependencies (system-spec)
  (let ((op 'compile-op)
        (system-name (if (and (listp system-spec) (eql (first system-spec) :version))
                         (progn
                           (check-dependency-versions system-spec)
                           (second system-spec))
                         system-spec)))
    (rest (assoc op (component-depends-on op (find-system system-name))))))

(defun check-dependency-versions (system-spec)
  (let* ((dep-name (second system-spec))
         (actual-version (component-version (find-system dep-name)))
         (needed-version (third system-spec)))
    (format t "~&System ~A needs version ~A. " dep-name needed-version)
    (if (string-equal actual-version needed-version)
        (format t "OK.")
        (format t "It is at version ~A." actual-version))))
