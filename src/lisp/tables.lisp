(in-package :scrooge)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-table-create/update/delete (table id (&rest fields))
    (with-gensyms (dao)
      (progn
	(let ((create-name (concatenate 'string "CREATE-" (symbol-name table)))
	      (update-name (concatenate 'string "UPDATE-" (symbol-name table)))
	      (delete-name (concatenate 'string "DELETE-" (symbol-name table)))
	      #|(supplied (mapcar (lambda (field)
				  (concatenate 'string (symbol-name field) "-S"))
				fields))|#) 
	  `(progn
	     ;; create
	     (defun ,(intern create-name) ,fields 
	       (with-db
		 (insert-dao
		  (make-instance ',table
				 ,@(mapcan (lambda (field)
					     `(,(make-keyword field) ,field))
					   fields)))))
	     ;; update
	     (defun ,(intern update-name) (,id &key ,@fields)
	       (with-db
		 (let ((,dao (or (get-dao ',table ,id)
				 (error "Row with ID = ~A not found in table ~A." ,id ',table))))
		   ,@(iter (for f in fields) 
			   (collect `(setf (,f ,dao) ,f)))
		   (update-dao ,dao))))
	     ;; delete
	     (defun ,(intern delete-name) (id)
	       (with-db
		 (let ((,dao (or (get-dao ',table ,id)
				 (error "Row with ID = ~A not found in table ~A." ,id ',table)))) 
		   (delete-dao ,dao))))
	     (list ,create-name ,update-name ,delete-name)))))))


;;; Auxiliary tables

(defclass webuser ()
  ((id       :col-type integer :accessor id)
   (username :col-type string  :accessor username :initarg :username) 
   (password :col-type string  :accessor password :initarg :password)
   (webrole  :col-type string  :accessor webrole  :initarg :webrole))
  (:metaclass dao-class)
  (:keys id))

(define-table-create/update/delete webuser id (username password webrole))

(defclass city ()
  ((id    :col-type integer :reader   id    :initarg :id)
   (title :col-type string  :accessor title :initarg :title))
  (:default-initargs :title nil)
  (:metaclass dao-class)
  (:keys id))

(define-table-create/update/delete city id (title))


(defclass tof () 
  ((id    :col-type integer :reader   id    :initarg :id)
   (title :col-type string  :accessor title :initarg :title))
  (:metaclass dao-class)
  (:keys id))

(define-table-create/update/delete tof id (title))


(defclass bank () 
  ((id    :col-type integer :reader   id    :initarg :id)
   (title :col-type string  :accessor title :initarg :title))
  (:metaclass dao-class)
  (:keys id))

(define-table-create/update/delete bank id (title))



;;; Accounts

(defclass account-group ()
  ((id       :col-type integer :reader   id)
   (basename :col-type string  :accessor basename :initarg :basename) 
   (title    :col-type string  :accessor title    :initarg :title)
   (debit-p  :col-type boolean :accessor debit-p  :initarg :debit-p))
  (:default-initargs :basename nil :title nil :debit-p nil)
  (:metaclass dao-class)
  (:keys id))

(define-table-create/update/delete account-group id (basename title debit-p))


(defclass account ()
  ((id        :col-type integer :reader   id)
   (parent-id :col-type integer :accessor parent-id :initarg :parent-id)
   (basename  :col-type string  :accessor basename  :initarg :basename)
   (title     :col-type string  :accessor title     :initarg :title) 
   (debit-p   :col-type boolean :accessor debit-p   :initarg :debit-p))
  (:default-initargs :basename nil :title nil :account-group-id nil)
  (:metaclass dao-class)
  (:keys id))

(define-table-create/update/delete account id
  (parent-id basename title debit-p))



;;; Companies

(defclass company ()
  ((id         :col-type integer :reader   id)
   (title      :col-type string  :accessor title      :initarg :title)
   (occupation :col-type string  :accessor occupation :initarg :occupation)
   (tof-id     :col-type integer :accessor tof-id     :initarg :tof-id)
   (tin        :col-type integer :accessor tin        :initarg :tin)
   (address    :col-type string  :accessor address    :initarg :address)
   (city-id    :col-type integer :accessor city-id    :initarg :city-id)
   (pobox      :col-type integer :accessor pobox      :initarg :pobox)
   (zipcode    :col-type integer :accessor zipcode    :initarg :zipcode))
  (:default-initargs :id nil :title nil :occupation nil
		     :tof-id nil :tin nil :address nil
		     :city-id nil :pobox nil :zipcode nil)
  (:metaclass dao-class)
  (:keys id))

(define-table-create/update/delete company id
  (title occupation tof-id tin address city-id pobox zipcode))



;;; Transactions

(defclass tx-type ()
  ((id            :col-type integer :reader   id) 
   (title         :col-type string  :accessor title         :initarg :title) 
   (debit-acc-id  :col-type integer :accessor debit-acc-id  :initarg :debit-acc-id)
   (credit-acc-id :col-type integer :accessor credit-acc-id :initarg :credit-acc-id))
  (:default-initargs :title nil :debit-acc-id nil :credit-acc-id nil)
  (:metaclass dao-class)
  (:keys id))

(define-table-create/update/delete tx-type id (title debit-acc-id credit-acc-id))


(defclass tx ()
  ((id         :col-type integer :reader   id) 
   (tx-date    :col-type date	 :accessor tx-date    :initarg :tx-date)
   (title      :col-type string	 :accessor title      :initarg :title)
   (tx-type-id :col-type integer :accessor tx-type-id :initarg :tx-type-id)
   (company-id :col-type integer :accessor company-id :initarg :company-id)
   (amount     :col-type integer :accessor amount     :initarg :amount))
  (:default-initargs :title nil :tx-date nil :amount nil :tx-type-id nil)
  (:metaclass dao-class)
  (:keys id))

(define-table-create/update/delete tx id (title tx-date amount tx-type-id company-id))



;;; Cheques

(defclass cheque ()
  ((id       :col-type integer :reader   id)
   (bank-id  :col-type integer :accessor bank-id  :initarg :bank-id)
   (tx-id    :col-type integer :accessor tx-id    :initarg :tx-id)
   (issuer   :col-type string  :accessor issuer   :initarg :issuer)
   (paid-p   :col-type boolean :accessor paid-p   :initarg :paid-p)
   (due-date :col-type date    :accessor due-date :initarg :due-date))
  (:default-initargs bank-id nil tx-id nil :issuer nil :paid-p "f" :due-date nil)
  (:metaclass dao-class)
  (:keys id))

(define-table-create/update/delete cheque id (bank-id tx-id issuer paid-p due-date))



;;; Projects

(defclass project ()
  ((id         :col-type integer :reader   id)
   (company-id :col-type integer :accessor company-id :initarg :company-id)
   (title      :col-type string  :accessor title      :initarg :title)
   (location   :col-type string  :accessor location   :initarg :location)
   (price      :col-type integer :accessor price      :initarg :price))
  (:metaclass dao-class)
  (:keys id))

(define-table-create/update/delete project id (company-id title location price))






    

