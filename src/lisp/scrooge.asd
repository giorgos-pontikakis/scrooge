;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(asdf:defsystem :scrooge
  :depends-on ("veil" "bricks" "mortar" "json")
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "tables")

               (:file "init")
               (:file "widgets")
               (:file "errors")
               ;; -- Here come the pages! --
               (:file "elements")
               (:file "auth")
               (:file "root")
               ;; ;; config
               (:file "config/config")
               (:file "config/bank")
               (:file "config/tof")
               (:file "config/city")
               (:file "config/account")
               ;; (:file "config/cheque-stran")
               (:file "config/option")
               ;; ;; admin
               (:file "admin/admin")
               (:file "admin/company")
               (:file "admin/contact")
               (:file "admin/project")
               (:file "admin/bill")
               ;; ;; financial
               (:file "financial/financial")
               ;; (:file "financial/tx")
               ;; (:file "financial/cash")
               ;; (:file "financial/invoice")
               ;; (:file "financial/cheque")
               ;; (:file "financial/account")
               ))