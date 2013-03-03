;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage :scrooge-asdf
  (:use :cl :asdf)
  (:export :deps :*scrooge-version*))

(in-package :scrooge-asdf)


(defvar *scrooge-version* "1.0.2"
  "A string denoting the current version of Scrooge.")

(export '*scrooge-version*)


(defsystem :scrooge
  :version #.*scrooge-version*
  :serial t
  ;;
  :depends-on ("ironclad"
               (:version "json" "1.0.0")
               (:version "lisputils" "1.0.0")
               (:version "veil" "1.0.0")
               (:version "mortar" "1.0.1")
               (:version "bricks" "1.0.2"))
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

(defun deps ()
  (mapcar (lambda (sys)
            (let ((ver (slot-value (asdf:find-system sys) 'asdf:version)))
              (format t "~A: ~A~&" sys ver)
              (list :version sys ver)))
          (list "json" "lisputils" "veil" "mortar" "bricks")))
