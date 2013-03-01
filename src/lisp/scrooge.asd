;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(defpackage :scrooge-asd
  (:use :cl :asdf))

(in-package :scrooge-asd)

(defvar *scrooge-version* "0.9.0"
  "A string denoting the current version of Scrooge.  Used
for diagnostic output.")

(export '*scrooge-version*)


(in-package :cl-user)

(asdf:defsystem :scrooge
  :depends-on ("ironclad" "lisputils" "veil" "bricks" "mortar" "json")
  :serial t
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
