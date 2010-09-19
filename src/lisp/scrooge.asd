;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(asdf:defsystem :scrooge
  :serial t
  :depends-on (:veil :simple-date :postmodern :metabang-bind :json)
  :components ((:file "package")
	       (:file "utils")
               (:file "tables")
	       (:file "init")
	       (:file "validators")
	       (:file "widgets")
	       (:file "snippets")
	       (:file "root")
	       (:file "config")
               (:file "banks")
               (:file "tofs")
               (:file "cities")
	       ;; (:file "companies")
	       ;; (:file "cheques")
	       ;; (:file "accounts")
	       ;; (:file "tx")
	       ;; (:file "temtx")
	       ;; (:file "stran")
	       ;; (:file "project") 
               ))