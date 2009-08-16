;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(asdf:defsystem :scrooge
    :serial t
    :depends-on (:www-toolkit)
    :components ((:file "package")
		 (:file "utils")
		 (:file "tables")
		 (:file "init")
		 (:file "widgets")
		 (:file "snippets")
		 (:file "actions")
		 (:file "pages")))




