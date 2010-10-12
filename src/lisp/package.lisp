(in-package :cl-user)

(defpackage :scrooge
  (:use :common-lisp
        :lisputils
        :hunchentoot
        :cl-who
        :cl-ppcre
        :iterate
        :alexandria
        :veil
        :simple-date
        :postmodern
        :json))
