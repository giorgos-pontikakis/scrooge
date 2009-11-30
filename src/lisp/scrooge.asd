;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(asdf:defsystem :scrooge
    :serial t
    :depends-on (:www-toolkit :simple-date :postmodern)
    :components ((:file "package") 
		 (:file "utils")
		 (:file "html-utils")
		 (:file "tables")
		 (:file "init")
		 (:file "widgets")
		 (:file "snippets")
		 (:file "validators")
		 (:file "login") 
		 (:file "root") 
		 (:file "accounts")
		 (:file "companies")
		 (:file "transactions")
		 (:file "transaction-types")
		 (:file "cheques")
		 (:file "actions")
		 (:file "pages"))) 