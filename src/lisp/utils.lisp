(in-package :scrooge)


;;;----------------------------------------------------------------------
;;; SQL utilities
;;;----------------------------------------------------------------------

(defun ilike (filter)
  (if (or (null filter)
          (eq filter :null))
      "%"
      (concatenate 'string "%" filter "%")))

(defmacro define-existence-predicate (name table field)
  `(defun ,name (,field)
     (with-db ()
       (query (:select 1 :from ',table :where (:= ',field ,field)) :single))))

(defmacro define-uniqueness-predicate (name table unique-field id-field)
  `(defun ,name (,unique-field &optional ,id-field)
     (with-db ()
       (not (if ,id-field
                (query (:select 1
                                :from ',table
                                :where (:and (:not (:= ',id-field ,id-field))
                                             (:= ',unique-field ,unique-field)))
                       :single)
                (query (:select 1
                                :from ',table
                                :where (:= ',unique-field ,unique-field))
                       :single))))))



;;;----------------------------------------------------------------------
;;; Miscellaneous
;;;----------------------------------------------------------------------

(defun full-url (page-name)
  (let ((page (find-page page-name *webapp*)))
    (concatenate 'string (webroot (webapp page)) (base-url page))))

(defun see-other (url)
  (redirect url :code +http-see-other+))

(defun true (x)
  (eq x t))

(defun params->plist (parameters)
  (iter (for p in parameters)
        (collect (key p))
        (collect (val* p))))
