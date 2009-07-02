(in-package :scrooge)

(disable-sql-reader-syntax)
(enable-sql-reader-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-table-create/update/delete (table id (&rest fields))
    ;; 
    ;; ATTENTION: In all update- defun defined with this macro:
    ;; 
    ;; If a keyword representing a table attribute is not present, it is
    ;; ignored. If it /is/ present /and/ it is NIL and there are no NOT
    ;; NULL constraint, the value NULL is inserted in the database
    ;;
    (with-gensyms (record)
      (progn
	(let ((create-name (concatenate 'string "CREATE-" (symbol-name table)))
	      (update-name (concatenate 'string "UPDATE-" (symbol-name table)))
	      (delete-name (concatenate 'string "DELETE-" (symbol-name table)))
	      (supplied (mapcar (lambda (field)
				  (concatenate 'string (symbol-name field) "-S"))
				fields))) 
	  `(progn
	     ;; create
	     (defun ,(intern create-name) ,fields 
	       (with-db
		 (update-records-from-instance
		  (make-instance ',table
				 ,@(mapcan (lambda (field)
					     `(,(make-keyword field) ,field))
					   fields)))))
	     ;; update
	     (defun ,(intern update-name) (,id &key ,@(iter (for f in fields)
							    (for s in supplied)
							    (collect `(,f nil ,(intern s)))))
	       (with-db
		 (let ((,record (or (select-unique ',table
						   :where [= [id] ,id]
						   :flatp t)
				    (error "Row with ID = ~A not found in table ~A." ,id ',table))))
		   ,@(iter (for f in fields)
			   (for s in supplied)
			   (collect `(when ,(intern s)
				       (setf (,f ,record) ,f))))
		   (update-records-from-instance ,record))))
	     ;; delete
	     (defun ,(intern delete-name) (id)
	       (with-db
		 (let ((,record (or (select-unique ',table
						   :where [= [id] ,id]
						   :flatp t)
				    (error "Row with ID = ~A not found in table ~A." ,id ',table))))
		   (delete-instance-records ,record))))
	     (list ,create-name ,update-name ,delete-name)))))))


;;; Auxiliary tables

(def-view-class city ()
  ((id    :type integer :reader   id    :db-kind :key :db-constraints :primary-key)
   (title :type string  :accessor title :initarg :title))
  (:default-initargs :title nil))

(define-table-create/update/delete city id (title))


(def-view-class tof () 
  ((id    :type integer :reader   id    :db-kind :key :db-constraints :primary-key)
   (title :type string  :accessor title :initarg :title)))

(define-table-create/update/delete tof id (title))


(def-view-class bank () 
  ((id    :type integer :reader   id    :db-kind :key :db-constraints :primary-key)
   (title :type string  :accessor title :initarg :title)))

(define-table-create/update/delete bank id (title))



;;; Accounts

(def-view-class account-group ()
  ((id              :type integer :reader   id              :db-kind :key :db-constraints :primary-key)
   (basename        :type string  :accessor basename        :initarg :basename) 
   (title           :type string  :accessor title           :initarg :title)
   (debit-account-p :type string  :accessor debit-account-p :initarg :debit-account-p))  ;; boolean-string
  (:default-initargs :basename nil :title nil :debit-account nil))

(define-table-create/update/delete account-group id (basename title debit-account-p))


(def-view-class account ()
  ((id               :type integer :reader   id               :db-kind :key :db-constraints :primary-key) 
   (basename         :type string  :accessor basename         :initarg :basename)
   (title            :type string  :accessor title            :initarg :title)
   (account-group-id :type integer :accessor account-group-id :initarg :account-group-id))
  (:default-initargs :basename nil :title nil :account-group-id nil))

(define-table-create/update/delete account id (basename title account-group-id))



;;; Companies

(def-view-class company ()
  ((id         :type integer :reader   id         :db-kind :key :db-constraints :primary-key)
   (title      :type string  :accessor title	  :initarg :title)
   (occupation :type string  :accessor occupation :initarg :occupation)
   (tof-id     :type integer :accessor tof-id	  :initarg :tof-id)
   (tin	       :type integer :accessor tin	  :initarg :tin)
   (address    :type string  :accessor address	  :initarg :address)
   (city-id    :type integer :accessor city-id	  :initarg :city-id)
   (pobox      :type integer :accessor pobox	  :initarg :pobox)
   (zipcode    :type integer  :accessor zipcode	  :initarg :zipcode))
  (:default-initargs
      :id nil :title nil :occupation nil
      :tof-id nil :tin nil :address nil
      :city-id nil :pobox nil :zipcode nil))

(define-table-create/update/delete company id (title occupation tof-id tin address city-id pobox zipcode))







;;; Transactions

(def-view-class tx-type ()
  ((id            :type integer :reader   id            :db-kind :key :db-constraints :primary-key) 
   (title         :type string  :accessor title         :initarg :title) 
   (debit-acc-id  :type integer :accessor debit-acc-id  :initarg :debit-acc-id)
   (credit-acc-id :type integer :accessor credit-acc-id :initarg :credit-acc-id))
  (:default-initargs :title nil :debit-acc-id nil :credit-acc-id nil))

(define-table-create/update/delete tx-type id (title debit-acc-id credit-acc-id))


(def-view-class tx ()
  ((id           :type integer :reader   id            :db-kind :key :db-constraints :primary-key) 
   (title        :type string  :accessor title         :initarg :title) 
   (txdate       :type date    :accessor txdate        :initarg :txdate)
   (amount       :type integer :accessor amount        :initarg :amount)
   (tx-group-id  :type integer :accessor tx-group-id   :initarg :tx-group-id))
  (:default-initargs :title nil :txdate nil :amount nil :tx-group-id nil))

(define-table-create/update/delete tx id (title txdate amount tx-group-id))



;;; Cheques

(def-view-class cheque ()
  ((id       :type integer :reader   id            :db-kind :key :db-constraints :primary-key)
   (bank-id  :type integer :accessor bank-id  :initarg :bank-id)
   (tx-id    :type integer :accessor tx-id    :initarg :tx-id)
   (issuer   :type string  :accessor issuer   :initarg :issuer)
   (paid-p   :type string  :accessor paid-p   :initarg :paid-p) ;; string-boolean
   (due-date :type date    :accessor due-date :initarg :due-date))
  (:default-initargs bank-id nil tx-id nil :issuer nil :paid-p "f" :due-date nil))

(define-table-create/update/delete cheque id (bank-id tx-id issuer paid-p due-date))



;;; Projects

(def-view-class project ()
  ((id         :type integer :reader   id         :db-kind :key        :db-constraints :primary-key)
   (company-id :type integer :accessor company-id :initarg :company-id)
   (title      :type string  :accessor title      :initarg :title)
   (location   :type string  :accessor location   :initarg :location)
   (price      :type integer :accessor price      :initarg :price)))

(define-table-create/update/delete project id (company-id title location price))