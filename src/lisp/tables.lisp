(in-package :scrooge)

(disable-sql-reader-syntax)
(enable-sql-reader-syntax)

(def-view-class account ()
  ((id               :type integer :reader   id               :db-kind :key              :db-constraints :primary-key)
   (basename         :type string  :accessor basename         :initarg :basename)
   (description      :type string  :accessor description      :initarg :description)
   (account-group-id :type integer :accessor account-group-id :initarg :account-group-id))
  (:default-initargs :basename nil :description nil :account-group-id nil))

(def-view-class account-group ()
  ((id            :type integer :reader   id            :db-kind :key           :db-constraints :primary-key)
   (debit-account :type string  :accessor debit-account :initarg :debit-account) ;; boolean-string
   (basename      :type string  :accessor basename      :initarg :basename)
   (description   :type string  :accessor description   :initarg :description))
  (:default-initargs :debit-account nil :basename nil :description nil))

(def-view-class tx-group ()
  ((id            :type integer     :reader   id            :db-kind :key :db-constraints :primary-key) 
   (title         :type (string 40) :accessor title         :initarg :title) 
   (txdate        :type string      :accessor txdate        :initarg :txdate)
   (amount        :type float       :accessor amount        :initarg :amount)
   (debit-acc-id  :type integer     :accessor debit-acc-id  :initarg :debit-acc-id)
   (credit-acc-id :type integer     :accessor credit-acc-id :initarg :credit-acc-id)))


;; Accounts
(defun create-account (basename description account-group-id)
  (with-db
    (update-records-from-instance
     (make-instance 'account
		    :basename basename
		    :description description
		    :account-group-id account-group-id))))

(defun edit-account (id &key basename description account-group-id)
  (with-db
    (let ((acc (or (select-unique 'account
				  :where [= [id] id]
				  :flatp t)
		   (error "Account not found"))))
      (when basename
	(setf (basename acc) basename))
      (when description
	(setf (description acc) description))
      (when account-group-id
	(setf (account-group-id acc) account-group-id))
      (update-records-from-instance acc))))

(defun delete-account (id)
  (with-db
    (let ((acc (or (select-unique 'account
				  :where [= [id] id]
				  :flatp t)
		   (error "Account not found"))))
      (delete-instance-records acc))))


;; Account Groups
(defun create-account-group (basename debit-account-p description)
  (with-db
    (update-records-from-instance
     (make-instance 'account-group
		    :basename basename
		    :debit-account (bool debit-account-p)
		    :description description))))

(defun edit-account-group (id &key basename description (debit-account-p nil debit-account-p-supplied))
  (with-db 
    (let ((acc-grp (or (select-unique 'account-group
				      :where [= [id] id]
				      :flatp t)
		       (error "Account group not found"))))
      (when basename
	(setf (basename acc-grp) basename))
      (when description
	(setf (description acc-grp) description))
      (when debit-account-p-supplied
	(setf (debit-account acc-grp) (bool debit-account-p))) 
      (update-records-from-instance acc-grp))))

(defun delete-account-group (id)
  (with-db
    (let ((acc-grp (or (select-unique 'account-group
				      :where [= [id] id]
				      :flatp t)
		       (error "Account group not found"))))
      (delete-instance-records acc-grp))))
