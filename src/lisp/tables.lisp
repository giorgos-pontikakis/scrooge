(in-package :scrooge)


;;; Interface to table rows

(defun create-row (table &rest fields)
  (with-db
    (insert-dao (apply #'make-instance table fields))))


(defun update-row (table primary-key &rest fields)
  ;; TODO: this does not work with composite primary keys
  (with-db
    (let ((pk (first (dao-keys table)))) 
      (execute
       (sql-compile
	`(:update ,table :set ,@fields :where (:= ,pk ,primary-key)))))))

(defun delete-row (table primary-key)
  (with-db
    (delete-dao (get-dao table primary-key))))



;;; Class definitions

(defclass bank () 
  ((id    :col-type string :accessor id    :initarg :id)
   (title :col-type string :accessor title :initarg :title))
  (:metaclass dao-class)
  (:keys id))

(defclass tof () 
  ((id    :col-type string :accessor id    :initarg :id)
   (title :col-type string :accessor title :initarg :title))
  (:metaclass dao-class)
  (:keys id))

(defclass company ()
  ((id         :col-type integer :reader   id)
   (title      :col-type string  :accessor title      :initarg :title)
   (occupation :col-type string  :accessor occupation :initarg :occupation)
   (tof-id     :col-type string  :accessor tof-id     :initarg :tof-id)
   (tin        :col-type string  :accessor tin        :initarg :tin)
   (address    :col-type string  :accessor address    :initarg :address)
   (city       :col-type string  :accessor city       :initarg :city)
   (pobox      :col-type integer :accessor pobox      :initarg :pobox)
   (zipcode    :col-type integer :accessor zipcode    :initarg :zipcode)) 
  (:default-initargs
      :id nil :title nil :occupation nil :tof-id nil :tin nil
      :address nil :city nil :pobox nil :zipcode nil)
  (:metaclass dao-class) 
  (:keys id))

(defclass cheque ()
  ((id         :col-type integer :reader   id) 
   (bank-id    :col-type string  :accessor bank-id    :initarg :bank-id) 
   (company-id :col-type integer :accessor company-id :initarg :company-id)
   (due-date   :col-type date    :accessor due-date   :initarg :due-date)
   (amount     :col-type integer :accessor amount     :initarg :amount)
   (status     :col-type string  :accessor status     :initarg :status)) 
  (:metaclass dao-class)
  (:keys id))

(defclass account ()
  ((id    :col-type integer :accessor id    :initarg :id)
   (title :col-type string  :accessor title :initarg :title))
  (:metaclass dao-class)
  (:keys id))

(defclass debit-account ()
  ((id        :col-type string :accessor id        :initarg :id)
   (parent-id :col-type string :accessor parent-id :initarg :parent-id) 
   #|(title     :col-type string  :accessor title    :initarg :title)|#) 
  (:metaclass dao-class)
  (:keys id))

(defclass credit-account ()
  ((id        :col-type string  :accessor id       :initarg :id)
   (parent-id :col-type string :accessor parent-id :initarg :parent-id) 
   #|(title     :col-type string  :accessor title    :initarg :title)|#) 
  (:metaclass dao-class)
  (:keys id))

(defclass tx ()
  ((id            :col-type integer :reader   id) 
   (tx-date       :col-type date    :accessor tx-date       :initarg :tx-date)
   (title         :col-type string  :accessor title         :initarg :title)
   (debit-acc-id  :col-type string  :accessor debit-acc-id  :initarg :debit-acc-id)
   (credit-acc-id :col-type string  :accessor credit-acc-id :initarg :credit-acc-id)
   (company-id    :col-type integer :accessor company-id    :initarg :company-id)
   (amount        :col-type integer :accessor amount        :initarg :amount)
   (cheque-id     :col-type integer :accessor cheque-id     :initarg :cheque-id)) 
  (:metaclass dao-class)
  (:keys id))

(defclass contact ()
  ((id         :col-type integer :reader   id)
   (company-id :col-type integer :accessor company-id :initarg :company-id)
   (tag        :col-type string  :accessor tag        :initarg :tag)
   (phone      :col-type string  :accessor phone      :initarg :phone))
  (:metaclass dao-class)
  (:keys id))






;;; Auxiliary tables

;; (defclass webuser ()
;;   ((id       :col-type integer :accessor id)
;;    (username :col-type string  :accessor username :initarg :username) 
;;    (password :col-type string  :accessor password :initarg :password)
;;    (webrole  :col-type string  :accessor webrole  :initarg :webrole))
;;   (:metaclass dao-class)
;;   (:keys id))

;; (define-table-create/update/delete webuser id (username password webrole))

;; (defclass city ()
;;   ((id    :col-type integer :reader   id    :initarg :id)
;;    (title :col-type string  :accessor title :initarg :title))
;;   (:default-initargs :title nil)
;;   (:metaclass dao-class)
;;   (:keys id))

;; (define-table-create/update/delete city id (title))





;;; Accounts

;; (defclass account-group ()
;;   ((id       :col-type integer :reader   id)
;;    (basename :col-type string  :accessor basename :initarg :basename) 
;;    (title    :col-type string  :accessor title    :initarg :title)
;;    (debit-p  :col-type boolean :accessor debit-p  :initarg :debit-p))
;;   (:default-initargs :basename nil :title nil :debit-p nil)
;;   (:metaclass dao-class)
;;   (:keys id))

;; (define-table-create/update/delete account-group id (basename title debit-p))


;; (defclass account ()
;;   ((id        :col-type integer :reader   id)
;;    (parent-id :col-type integer :accessor parent-id :initarg :parent-id) 
;;    (title     :col-type string  :accessor title     :initarg :title) 
;;    (debit-p   :col-type boolean :accessor debit-p   :initarg :debit-p))
;;   (:default-initargs :title nil :account-group-id nil)
;;   (:metaclass dao-class)
;;   (:keys id))

;; (define-table-create/update/delete account id
;;   (parent-id title debit-p))




;; (define-table-create/update/delete company id
;;   (title occupation tof-id tin address city-id pobox zipcode))



;;; Transactions

;; (defclass tx-type ()
;;   ((id            :col-type integer :reader   id) 
;;    (title         :col-type string  :accessor title         :initarg :title) 
;;    (debit-acc-id  :col-type integer :accessor debit-acc-id  :initarg :debit-acc-id)
;;    (credit-acc-id :col-type integer :accessor credit-acc-id :initarg :credit-acc-id))
;;   (:default-initargs :title nil :debit-acc-id nil :credit-acc-id nil)
;;   (:metaclass dao-class)
;;   (:keys id))

;; (define-table-create/update/delete tx-type id (title debit-acc-id credit-acc-id))


;; (defclass tx ()
;;   ((id            :col-type integer :reader   id) 
;;    (tx-date       :col-type date    :accessor tx-date       :initarg :tx-date)
;;    (title         :col-type string  :accessor title         :initarg :title)
;;    (debit-acc-id  :col-type integer :accessor debit-acc-id  :initarg :debit-acc-id)
;;    (credit-acc-id :col-type integer :accessor credit-acc-id :initarg :credit-acc-id)
;;    (company-id    :col-type integer :accessor company-id    :initarg :company-id)
;;    (amount        :col-type integer :accessor amount        :initarg :amount)
;;    (cheque-id     :col-type integer :accessor cheque-id     :initarg :cheque-id))
;;   (:default-initargs :title nil :tx-date nil :amount nil :tx-type-id nil)
;;   (:metaclass dao-class)
;;   (:keys id))

;; (define-table-create/update/delete tx id (title tx-date amount debit-acc-id credit-acc-id company-id cheque-id))



;;; Cheques


;; (define-table-create/update/delete cheque id (issuer-id payee-id bank-id due-date amount))



;;; Projects

;; (defclass project ()
;;   ((id         :col-type integer :reader   id)
;;    (company-id :col-type integer :accessor company-id :initarg :company-id)
;;    (title      :col-type string  :accessor title      :initarg :title)
;;    (location   :col-type string  :accessor location   :initarg :location)
;;    (price      :col-type integer :accessor price      :initarg :price))
;;   (:metaclass dao-class)
;;   (:keys id))

;; (define-table-create/update/delete project id (company-id title location price))






    

