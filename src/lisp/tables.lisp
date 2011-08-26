(in-package :scrooge)


;;; ------------------------------------------------------------
;;; Config
;;; ------------------------------------------------------------

(defclass option ()
  ((option-id    :col-type (string 32)  :reader   option-id)
   (lisp-type    :col-type (string 16)  :reader   lisp-type)
   (config-value :col-type (string 128) :accessor config-value :initarg :config-value))
  (:metaclass dao-class)
  (:keys option-id))



(defclass usr ()
  ((username  :col-type (string 128) :accessor username  :initarg :username)
   (password  :col-type (string 128) :accessor password  :initarg :password)
   (authgroup :col-type (string 128) :accessor authgroup :initarg :authgroup))
  (:metaclass dao-class)
  (:keys username))


(defclass bank ()
  ((bank-id :col-type integer :reader   bank-id)
   (title   :col-type string  :accessor title   :initarg :title))
  (:metaclass dao-class)
  (:keys bank-id))

(defmethod bank-id ((title string))
  (with-db ()
    (query (:select 'id :from 'bank :where (:= 'title title))
           :single)))



(defclass tof ()
  ((tof-id :col-type integer :reader   tof-id)
   (title  :col-type string  :accessor title  :initarg :title))
  (:metaclass dao-class)
  (:keys tof-id))

(defmethod tof-id ((tof-title string))
  (with-db ()
    (query (:select 'tof-id :from 'tof :where (:= 'title tof-title))
           :single)))



(defclass city ()
  ((city-id    :col-type integer :reader   city-id)
   (title :col-type string  :accessor title :initarg :title))
  (:metaclass dao-class)
  (:keys city-id))

(defmethod city-id ((title string))
  (with-db ()
    (query (:select 'city-id :from 'city :where (:= 'title title))
           :single)))



(defclass cheque-status ()
  ((status      :col-type string :reader   status)
   (description :col-type string :accessor description :initarg :description))
  (:metaclass dao-class)
  (:keys status))



(defclass cheque-stran ()
  ((cheque-stran-id :col-type integer :accessor cheque-stran-id :initarg :id)
   (title           :col-type string  :accessor title           :initarg :title)
   (debit-acc-id    :col-type integer :accessor debit-acc-id    :initarg :debit-acc-id)
   (credit-acc-id   :col-type integer :accessor credit-acc-id   :initarg :credit-acc-id)
   (payable-p       :col-type boolean :accessor payable-p       :initarg :payable-p)
   (from-status     :col-type string  :accessor from-status     :initarg :from-status)
   (to-status       :col-type string  :accessor to-status       :initarg :to-status))
  (:metaclass dao-class)
  (:keys cheque-stran-id))



(defclass account ()
  ((account-id         :col-type string  :accessor account-id         :initarg :account-id)
   (title      :col-type string  :accessor title      :initarg :title)
   (debit-p    :col-type boolean :accessor debit-p    :initarg :debit-p)
   (parent-id  :col-type string  :accessor parent-id  :initarg :parent-id)
   (chequing-p :col-type boolean :accessor chequing-p :initarg :chequing-p)
   (rank       :col-type integer :accessor rank       :initarg :rank))
  (:metaclass dao-class)
  (:keys account-id))

(defmethod account-id ((title string))
  (with-db ()
    (query (:select 'account-id :from 'account :where (:= 'title title))
           :single)))

(defmethod chequing-p ((title string))
  (with-db ()
    (query (:select 'account-id
                    :from 'account
                    :where (:and (:= 'title title)
                                 (:= 'chequing-p t)))
           :plists)))

(defmethod debit-p ((acc-id integer))
  (with-db ()
    (query (:select 'debit-p :from 'account :where (:= 'account-id acc-id))
           :single)))



;;; ------------------------------------------------------------
;;; Admin
;;; ------------------------------------------------------------

(defclass company ()
  ((company-id :col-type integer :reader   company-id)
   (title      :col-type string  :accessor title      :initarg :title)
   (occupation :col-type string  :accessor occupation :initarg :occupation)
   (tof-id     :col-type integer :accessor tof-id     :initarg :tof-id)
   (tin        :col-type string  :accessor tin        :initarg :tin)
   (address    :col-type string  :accessor address    :initarg :address)
   (city-id    :col-type integer :accessor city-id    :initarg :city-id)
   (pobox      :col-type integer :accessor pobox      :initarg :pobox)
   (zipcode    :col-type integer :accessor zipcode    :initarg :zipcode)
   (notes      :col-type string  :accessor notes      :initarg :notes))
  (:metaclass dao-class)
  (:keys company-id))

(defmethod company-id ((title string))
  (with-db ()
    (query (:select 'company-id :from 'company
                    :where (:= 'title title))
           :single)))



(defclass project ()
  ((project-id  :col-type integer       :reader   project-id)
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
  (:keys project-id))

(defclass contact ()
  ((contact-id :col-type integer :reader   contact-id)
   (company-id :col-type integer :accessor company-id :initarg :company-id)
   (tag        :col-type string  :accessor tag        :initarg :tag)
   (phone      :col-type string  :accessor phone      :initarg :phone))
  (:metaclass dao-class)
  (:keys contact-id))



;;; ------------------------------------------------------------
;;; Financial
;;; ------------------------------------------------------------

(defclass cheque ()
  ((cheque-id  :col-type integer       :reader   cheque-id)
   (bank-id    :col-type string        :accessor bank-id    :initarg :bank-id)
   (company-id :col-type integer       :accessor company-id :initarg :company-id)
   (due-date   :col-type date          :accessor due-date   :initarg :due-date)
   (amount     :col-type (numeric 9 2) :accessor amount     :initarg :amount)
   (status     :col-type string        :accessor status     :initarg :status)
   (payable-p  :col-type boolean       :accessor payable-p  :initarg :payable-p))
  (:metaclass dao-class)
  (:keys cheque-id))

(defclass tx ()
  ((tx-id         :col-type integer       :reader   tx-id)
   (tx-date       :col-type date          :accessor tx-date       :initarg :tx-date)
   (description   :col-type string        :accessor description   :initarg :description)
   (debit-acc-id  :col-type string        :accessor debit-acc-id  :initarg :debit-acc-id)
   (credit-acc-id :col-type string        :accessor credit-acc-id :initarg :credit-acc-id)
   (company-id    :col-type integer       :accessor company-id    :initarg :company-id)
   (cheque-id     :col-type integer       :accessor cheque-id     :initarg :cheque-id)
   (amount        :col-type (numeric 9 2) :accessor amount        :initarg :amount))
  (:metaclass dao-class)
  (:keys tx-id))
