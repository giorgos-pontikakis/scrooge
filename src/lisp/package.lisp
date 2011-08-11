(in-package :cl-user)

(defpackage :scrooge
  (:use #:common-lisp #:iterate #:alexandria
        #:hunchentoot #:cl-who #:cl-ppcre
        #:postmodern #:simple-date
        #:lisputils #:veil #:bricks #:mortar #:json))
