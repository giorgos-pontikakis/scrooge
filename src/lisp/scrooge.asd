;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(asdf:defsystem :scrooge
  :serial t
  :depends-on (:veil :bricks :json)
  :components ((:file "package")
               (:file "init")
               (:file "utils")
               (:file "tables")
               (:file "widgets")
               (:file "elements")
               (:file "root")
               ;; config
               (:file "config/config")
               (:file "config/bank")
               (:file "config/tof")
               (:file "config/city")
               (:file "config/account")
               #|(:file "config/temtx")|#
               ;; main
               (:file "main/company")
               #|(:file "main/cheque")|#
               #|(:file "main/project")|#
               #|(:file "main/tx")|#))