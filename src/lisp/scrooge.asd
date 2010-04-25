;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(asdf:defsystem :scrooge
  :serial t
  :depends-on (:www-toolkit :simple-date :postmodern :metabang-bind)
  :components ((:file "package")
	       (:file "utils")
	       (:file "init")
	       (:file "validators")
	       (:file "widgets")
	       (:file "snippets")
	       (:file "tables")
	       (:file "root")
	       (:file "config")
	       (:file "companies")
	       (:file "cheques")
	       (:file "accounts")
	       (:file "tx")
	       (:file "temtx")
	       (:file "fsm")))