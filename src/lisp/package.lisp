(in-package :cl-user)

(defpackage :scrooge
  (:use :common-lisp
	:hunchentoot
	:cl-who
	:cl-ppcre
	:iterate
	:alexandria
	:simple-date
	:postmodern
	:www-toolkit
	:metabang-bind
	:json))

