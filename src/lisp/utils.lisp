(in-package :scrooge)


;;;----------------------------------------------------------------------
;;; SQL utilities
;;;----------------------------------------------------------------------

(defun ilike (filter)
  (if (or (null filter)
          (eq filter :null))
      "%"
      (concatenate 'string "%" filter "%")))


;;;----------------------------------------------------------------------
;;; Miscellaneous 
;;;----------------------------------------------------------------------
(defun see-other (url)
  (hunchentoot:redirect url :code hunchentoot:+http-see-other+))


