;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(asdf:defsystem :scrooge
    :serial t
    :depends-on (:www)
    :components ((:file "package")
		 (:file "utils")
		 (:file "init")
		 (:file "tables")
		 (:file "core")))




