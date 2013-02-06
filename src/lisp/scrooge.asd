;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(asdf:defsystem :scrooge
  :depends-on ("ironclad" "lisputils" "veil" "bricks" "mortar" "json")
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "tables")
               (:file "init")
               (:file "globals")
               (:file "sql")
               (:file "family")
               (:file "widgets")
               (:file "errors")
               ;; -- Here come the pages! --
               (:file "elements")
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
               ;; basics
               (:file "main/company")
               (:file "main/contact")
               (:file "main/project")
               (:file "main/bill")
               ;; tx basics
               (:file "advanced/tx")
               (:file "advanced/account")
               ;; tx details
               (:file "main/cash")
               (:file "main/invoice")
               (:file "main/cheque")
               (:file "main/libtx")
               (:file "main/company-tx")
               (:file "main/company-cheque")
               ;; root
               (:file "root")))
