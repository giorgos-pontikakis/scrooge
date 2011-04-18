(in-package :scrooge)



;;; Config

(defclass option ()
  ((id           :col-type (string 32)  :reader   id)
   (lisp-type    :col-type (string 16)  :reader   lisp-type)
   (config-value :col-type (string 128) :accessor config-value :initarg :config-value))
  (:metaclass dao-class)
  (:keys id))

(defclass usr ()
  ((username  :col-type (string 128) :accessor username  :initarg :username)
   (password  :col-type (string 128) :accessor password  :initarg :password)
   (authgroup :col-type (string 128) :accessor authgroup :initarg :authgroup))
  (:metaclass dao-class)
  (:keys username))


(defclass bank ()
  ((id    :col-type integer :reader   id)
   (title :col-type string  :accessor title :initarg :title))
  (:metaclass dao-class)
  (:keys id))

(defmethod bank-id ((title string))
  (with-db ()
    (query (:select 'id :from 'bank :where (:= 'title title))
           :single)))

(defmethod bank-id ((title (eql :null)))
  :null)



(defclass tof ()
  ((id    :col-type integer :reader id)
   (title :col-type string :accessor title :initarg :title))
  (:metaclass dao-class)
  (:keys id))

(defmethod tof-id ((tof-title string))
  (with-db ()
    (query (:select 'id :from 'tof :where (:= 'title tof-title))
           :single)))

(defmethod tof-id ((tof-title (eql :null)))
  :null)



(defclass city ()
  ((id    :col-type integer :reader   id)
   (title :col-type string  :accessor title :initarg :title))
  (:metaclass dao-class)
  (:keys id))

(defmethod city-id ((title string))
  (with-db ()
    (query (:select 'id :from 'city :where (:= 'title title))
           :single)))

(defmethod city-id ((city-title (eql :null)))
  :null)



(defclass cheque-status ()
  ((status      :col-type string :reader   status)
   (description :col-type string :accessor description :initarg :description))
  (:metaclass dao-class)
  (:keys status))

(defclass cheque-stran ()
  ((id            :col-type integer :accessor id            :initarg :id)
   (title         :col-type string  :accessor title         :initarg :title)
   (debit-acc-id  :col-type integer :accessor debit-acc-id  :initarg :debit-acc-id)
   (credit-acc-id :col-type integer :accessor credit-acc-id :initarg :credit-acc-id)
   (payable-p     :col-type boolean :accessor payable-p     :initarg :payable-p)
   (from-status   :col-type string  :accessor from-status   :initarg :from-status)
   (to-status     :col-type string  :accessor to-status     :initarg :to-status))
  (:metaclass dao-class)
  (:keys id))

(defclass account ()
  ((id         :col-type string  :accessor id         :initarg :id)
   (title      :col-type string  :accessor title      :initarg :title)
   (debit-p    :col-type boolean :accessor debit-p    :initarg :debit-p)
   (parent-id  :col-type string  :accessor parent-id  :initarg :parent-id)
   (chequing-p :col-type boolean :accessor chequing-p :initarg :chequing-p))
  (:metaclass dao-class)
  (:keys id))

(defmethod chequing-p ((title string))
  (with-db ()
    (query (:select 'id
                    :from 'account
                    :where (:and (:= 'title title)
                                 (:= 'chequing-p t)))
           :plists)))

(defgeneric account-id (title)
  (:documentation "Given the title of the account, get the account id"))

(defmethod account-id ((title string))
  (with-db ()
    (query (:select 'id :from 'account :where (:= 'title title))
           :single)))

(defmethod account-id ((title (eql :null)))
  :null)

(defmethod debit-p ((acc-id integer))
  (with-db ()
    (query (:select 'debit-p :from 'account :where (:= 'id acc-id))
           :single)))



;;; Admin

(defclass company ()
  ((id         :col-type integer :reader   id)
   (title      :col-type string  :accessor title      :initarg :title)
   (occupation :col-type string  :accessor occupation :initarg :occupation)
   (tof-id     :col-type integer :accessor tof-id     :initarg :tof-id)
   (tin        :col-type string  :accessor tin        :initarg :tin)
   (address    :col-type string  :accessor address    :initarg :address)
   (city-id    :col-type integer :accessor city-id    :initarg :city-id)
   (pobox      :col-type integer :accessor pobox      :initarg :pobox)
   (zipcode    :col-type integer :accessor zipcode    :initarg :zipcode)
   (notes      :col-type string  :accessor notes      :initarg :notes))
  (:default-initargs
   :id nil :title nil :occupation nil :tof-id nil :tin nil
   :address nil :city nil :pobox nil :zipcode nil)
  (:metaclass dao-class)
  (:keys id))

(defmethod company-id ((title string))
  (with-db ()
    (query (:select 'id :from 'company
                    :where (:= 'title title))
           :single)))

(defmethod company-id ((title (eql :null)))
  :null)

(defclass project ()
  ((id          :col-type integer       :reader   id)
   (company-id  :col-type integer       :accessor company-id  :initarg :company-id)
   (description :col-type string        :accessor description :initarg :description)
   (location    :col-type string        :accessor location    :initarg :location)
   (price       :col-type (numeric 9 2) :accessor price       :initarg :price)
   (quote-date  :col-type date          :accessor quote-date  :initarg :quote-date)
   (start-date  :col-type date          :accessor start-date  :initarg :start-date)
   (end-date    :col-type date          :accessor end-date    :initarg :end-date)
   (status      :col-type string        :accessor status      :initarg :status)
   (vat         :col-type (numeric 9 2) :accessor vat         :initarg :vat)
   (notes       :col-type string        :accessor notes       :initarg :notes))
  (:metaclass dao-class)
  (:keys id))

(defclass contact ()
  ((id         :col-type integer :reader   id)
   (company-id :col-type integer :accessor company-id :initarg :company-id)
   (tag        :col-type string  :accessor tag        :initarg :tag)
   (phone      :col-type string  :accessor phone      :initarg :phone))
  (:metaclass dao-class)
  (:keys id))



;;; Financial

(defclass cheque ()
  ((id         :col-type integer       :reader   id)
   (bank-id    :col-type string        :accessor bank-id    :initarg :bank-id)
   (company-id :col-type integer       :accessor company-id :initarg :company-id)
   (due-date   :col-type date          :accessor due-date   :initarg :due-date)
   (amount     :col-type (numeric 9 2) :accessor amount     :initarg :amount)
   (status     :col-type string        :accessor status     :initarg :status)
   (payable-p  :col-type boolean       :accessor payable-p  :initarg :payable-p))
  (:metaclass dao-class)
  (:keys id))

(defclass tx ()
  ((id            :col-type integer       :reader   id)
   (tx-date       :col-type date          :accessor tx-date       :initarg :tx-date)
   (description   :col-type string        :accessor description   :initarg :description)
   (debit-acc-id  :col-type string        :accessor debit-acc-id  :initarg :debit-acc-id)
   (credit-acc-id :col-type string        :accessor credit-acc-id :initarg :credit-acc-id)
   (company-id    :col-type integer       :accessor company-id    :initarg :company-id)
   (cheque-id     :col-type integer       :accessor cheque-id     :initarg :cheque-id)
   (amount        :col-type (numeric 9 2) :accessor amount        :initarg :amount))
  (:metaclass dao-class)
  (:keys id))
