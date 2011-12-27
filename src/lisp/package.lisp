(in-package :cl-user)

(defpackage :scrooge
  (:use #:common-lisp #:iterate #:alexandria
        #:hunchentoot #:cl-who #:cl-ppcre
        #:postmodern #:local-time
        #:lisputils #:veil #:bricks #:mortar #:json))
