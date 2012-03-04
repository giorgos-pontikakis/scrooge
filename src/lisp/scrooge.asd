;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(asdf:defsystem :scrooge
  :depends-on ("veil" "bricks" "mortar" "json")
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "tables")
               (:file "init")
               (:file "globals")
               (:file "widgets")
               (:file "errors")
               ;; -- Here come the pages! --
               (:file "elements")
               (:file "auth")
               (:file "root")
               ;; config
               (:file "config/config")
               (:file "config/bank")
               (:file "config/tof")
               (:file "config/city")
               (:file "config/account")
               (:file "config/role")
               (:file "config/temtx")
               (:file "config/cheque-stran")
               ;; (:file "config/project-stran")
               ;; main
               (:file "main/main")
               (:file "main/home")
               (:file "main/company")
               (:file "main/contact")
               (:file "main/project")
               (:file "main/bill")
               (:file "main/tx")
               (:file "main/cash")
               (:file "main/invoice")
               (:file "main/cheque")
               ;; (:file "main/account")
               ))