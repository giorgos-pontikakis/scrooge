;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(asdf:defsystem :scrooge
    :serial t
    :depends-on (:www-toolkit :postmodern :cl-postgres :simple-date)
    :components ((:file "package") 
		 (:file "utils")
		 (:file "html-utils")
		 (:file "tables")
		 (:file "init")
		 (:file "widgets")
		 (:file "snippets")
		 (:file "validators")
		 (:file "companies")
		 (:file "transactions")
		 (:file "actions")
		 (:file "pages"))) 