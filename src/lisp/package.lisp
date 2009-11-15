(in-package :cl-user)

(defpackage :scrooge
  (:use :common-lisp
	:hunchentoot
	:cl-who
	:cl-ppcre
	:iterate
	:alexandria
	:postmodern
	:cl-postgres
	:simple-date
	:www-toolkit))

