(in-package :scrooge)

(disable-sql-reader-syntax)
(enable-sql-reader-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-table-create/update/delete (table id (&rest fields))
    ;; 
    ;; ATTENTION: In every update- defun defined with this macro:
    ;; 
    ;; If a keyword representing a table attribute is not present, it is
    ;; ignored. If it /is/ present /and/ it is NIL and there are no NOT
    ;; NULL constraints, the value NULL is inserted in the database
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

(def-view-class webuser ()
  ((id       :type integer :accessor id       :db-kind :key  :initarg :id)
   (username :type string  :accessor username :db-kind :base :initarg :username) 
   (password :type string  :accessor password :db-kind :base :initarg :password)
   (webrole  :type string  :accessor webrole  :db-kind :base :initarg :webrole)))

(define-table-create/update/delete webuser id (username password webrole))

(def-view-class city ()
  ((id    :type integer :reader   id    :db-kind :key  :db-constraints :primary-key)
   (title :type string  :accessor title :db-kind :base :initarg :title))
  (:default-initargs :title nil))

(define-table-create/update/delete city id (title))


(def-view-class tof () 
  ((id    :type integer :reader   id    :db-kind :key  :db-constraints :primary-key)
   (title :type string  :accessor title :db-kind :base :initarg :title)))

(define-table-create/update/delete tof id (title))


(def-view-class bank () 
  ((id    :type integer :reader   id    :db-kind :key  :db-constraints :primary-key)
   (title :type string  :accessor title :db-kind :base :initarg :title)))

(define-table-create/update/delete bank id (title))



;;; Accounts

(def-view-class account-group ()
  ((id              :type integer :reader   id              :db-kind :key  :db-constraints :primary-key)
   (basename        :type string  :accessor basename        :db-kind :base :initarg :basename) 
   (title           :type string  :accessor title           :db-kind :base :initarg :title)
   (debit-account-p :type string  :accessor debit-account-p :db-kind :base :initarg :debit-account-p))  ;; boolean-string
  (:default-initargs :basename nil :title nil :debit-account nil))

(define-table-create/update/delete account-group id (basename title debit-account-p))


(def-view-class account ()
  ((id	      :type integer :reader   id	:db-kind :key :db-constraints :primary-key)
   (parent-id :type integer :accessor parent-id	:db-kind :base :initarg :parent-id)
   (basename  :type string  :accessor basename	:db-kind :base :initarg :basename)
   (title     :type string  :accessor title	:db-kind :base :initarg :title) 
   (debit-p   :type boolean :accessor debit-p   :db-kind :base :initarg :debit-p))
  (:default-initargs :basename nil :title nil :account-group-id nil))

(define-table-create/update/delete account id
  (parent-id basename title debit-p))



;;; Companies

(def-view-class company ()
  ((id         :type integer :reader   id         :db-kind :key  :db-constraints :primary-key)
   (title      :type string  :accessor title	  :db-kind :base :initarg :title)
   (occupation :type string  :accessor occupation :db-kind :base :initarg :occupation)
   (tof-id     :type integer :accessor tof-id	  :db-kind :base :initarg :tof-id)
   (tin	       :type integer :accessor tin	  :db-kind :base :initarg :tin)
   (address    :type string  :accessor address	  :db-kind :base :initarg :address)
   (city-id    :type integer :accessor city-id	  :db-kind :base :initarg :city-id)
   (pobox      :type integer :accessor pobox	  :db-kind :base :initarg :pobox)
   (zipcode    :type integer :accessor zipcode	  :db-kind :base :initarg :zipcode))
  (:default-initargs
      :id nil :title nil :occupation nil
      :tof-id nil :tin nil :address nil
      :city-id nil :pobox nil :zipcode nil))

(define-table-create/update/delete company id
  (title occupation tof-id tin address city-id pobox zipcode))



;;; Transactions

(def-view-class tx-type (standard-db-object)
  ((id            :type integer :reader   id            :db-kind :key  :db-constraints :primary-key) 
   (title         :type string  :accessor title         :db-kind :base :initarg :title) 
   (debit-acc-id  :type integer :accessor debit-acc-id  :db-kind :base :initarg :debit-acc-id)
   (credit-acc-id :type integer :accessor credit-acc-id :db-kind :base :initarg :credit-acc-id))
  (:default-initargs :title nil :debit-acc-id nil :credit-acc-id nil))

(define-table-create/update/delete tx-type id (title debit-acc-id credit-acc-id))


(def-view-class tx ()
  ((id           :type integer :reader   id            :db-kind :key  :db-constraints :primary-key) 
   (tx-date      :type date    :accessor tx-date       :db-kind :base :initarg :tx-date)
   (title        :type string  :accessor title         :db-kind :base :initarg :title)
   (amount       :type integer :accessor amount        :db-kind :base :initarg :amount)
   (tx-type-id   :type integer :accessor tx-type-id    :db-kind :base :initarg :tx-type-id))
  (:default-initargs :title nil :tx-date nil :amount nil :tx-type-id nil))

(define-table-create/update/delete tx id (title tx-date amount tx-type-id))



;;; Cheques

(def-view-class cheque ()
  ((id       :type integer :reader   id       :db-kind :key  :db-constraints :primary-key)
   (bank-id  :type integer :accessor bank-id  :db-kind :base :initarg :bank-id)
   (tx-id    :type integer :accessor tx-id    :db-kind :base :initarg :tx-id)
   (issuer   :type string  :accessor issuer   :db-kind :base :initarg :issuer)
   (paid-p   :type string  :accessor paid-p   :db-kind :base :initarg :paid-p) ;; string-boolean
   (due-date :type date    :accessor due-date :db-kind :base :initarg :due-date))
  (:default-initargs bank-id nil tx-id nil :issuer nil :paid-p "f" :due-date nil))

(define-table-create/update/delete cheque id (bank-id tx-id issuer paid-p due-date))



;;; Projects

(def-view-class project ()
  ((id         :type integer :reader   id         :db-kind :key  :db-constraints :primary-key)
   (company-id :type integer :accessor company-id :db-kind :base :initarg :company-id)
   (title      :type string  :accessor title      :db-kind :base :initarg :title)
   (location   :type string  :accessor location   :db-kind :base :initarg :location)
   (price      :type integer :accessor price      :db-kind :base :initarg :price)))

(define-table-create/update/delete project id (company-id title location price))