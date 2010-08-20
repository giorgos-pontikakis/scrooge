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
  (redirect url :code +http-see-other+))


