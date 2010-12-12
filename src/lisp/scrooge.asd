;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(asdf:defsystem :scrooge
  :serial t
  :depends-on (:veil :bricks)
  :components ((:file "package")
               (:file "init")
               (:file "utils")
               (:file "tables")

               ;; (:file "validators")
               (:file "widgets")
               (:file "snippets")
               (:file "root")
               (:file "config")
               (:file "bank")
               ;; (:file "tof")
               ;; (:file "city")
               ;; (:file "account")
               ;; (:file "stran")
               ;; (:file "company")
               ;; (:file "cheque")
               ;; (:file "tx")
               ;; (:file "temtx")
               ;; (:file "project")
               ))