;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(asdf:defsystem :scrooge
  :serial t
  :depends-on (:veil :bricks :json :md5)
  :components ((:file "package")
               (:file "utils")
               (:file "auth")
               (:file "tables")
               (:file "widgets")
               (:file "elements")
               (:file "init")
               ;; -- Here come the pages! --
               (:file "root")
               ;; config
               (:file "config/config")
               (:file "config/bank")
               (:file "config/tof")
               (:file "config/city")
               (:file "config/account")
               (:file "config/cheque-stran")
               (:file "config/option")
               ;; admin
               (:file "admin/admin")
               (:file "admin/company")
               (:file "admin/contact")
               (:file "admin/project")
               ;; financial
               (:file "financial/financial")
               (:file "financial/tx")
               (:file "financial/cheque")
               (:file "financial/account")
               (:file "financial/cash")
               (:file "financial/invoice")))