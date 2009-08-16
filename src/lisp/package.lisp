(in-package :cl-user)

(defpackage :scrooge
  (:use :common-lisp
	:hunchentoot
	:cl-who
	:cl-ppcre
	:iterate
	:clsql
	:clsql-postgresql
	:alexandria
	:www-toolkit))
