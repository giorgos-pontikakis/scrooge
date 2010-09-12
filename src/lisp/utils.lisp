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

(defun find-datum (plist key)
  (if plist
      (getf plist key)
      nil))


(defun true (x)
  (eql x t))

(defun params->plist (parameters)
  (iter (for p in parameters)
        (collect (make-keyword (name p)))
        (collect (val* p))))