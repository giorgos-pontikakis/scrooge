(in-package :scrooge)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-alternate-key-readers (table (&rest alternate-key-types) &rest slots)
    `(progn
       ,@(mapcar (lambda (slot)
                   `(defmethod ,slot ,alternate-key-types
                      (with-db ()
                        (,slot (select-dao-unique
                                   ',table
                                   (:and ,@(mapcar (lambda (alternate-key)
                                                     `(:= ',alternate-key ,alternate-key))
                                                   (mapcar #'first alternate-key-types))))))))
                 slots))))



;;; CONFIGURATION

(defclass usr ()
  ((username  :col-type string :accessor username  :initarg :username)
   (password  :col-type string :accessor password  :initarg :password)
   (authgroup :col-type string :accessor authgroup :initarg :authgroup))
  (:metaclass dao-class)
  (:keys username))

(defclass bank ()
  ((id    :col-type integer :reader   bank-id)
   (title :col-type string  :accessor title   :initarg :title))
  (:metaclass dao-class)
  (:keys id)
  (:default-initargs :title nil))

(define-alternate-key-readers bank ((title string)) bank-id)

(defmethod bank-id ((title (eql :null)))
  :null)


(defclass tof ()
  ((id    :col-type integer :reader   tof-id)
   (title :col-type string  :accessor title  :initarg :title))
  (:metaclass dao-class)
  (:keys id)
  (:default-initargs :title nil))

(define-alternate-key-readers tof ((title string)) tof-id)

(defmethod tof-id ((title (eql :null)))
  :null)


(defclass city ()
  ((id    :col-type integer :reader   city-id)
   (title :col-type string  :accessor title :initarg :title))
  (:metaclass dao-class)
  (:keys id)
  (:default-initargs :title nil))

(define-alternate-key-readers city ((title string)) city-id)

(defmethod city-id ((title (eql :null)))
  :null)


(defclass account-role ()
  ((id          :col-type string  :reader   account-role-id)
   (description :col-type string  :reader   description)
   (rank        :col-type integer :reader   rank)
   (account-id  :col-type integer :accessor account-id      :initarg :account-id))
  (:metaclass dao-class)
  (:keys id))

(defmethod account-id ((role symbol))
  (with-db ()
    (account-id (get-dao 'account-role (string-downcase role)))))

(defmethod account-id ((role null))
  (declare (ignore role))
  (error "Account role cannot be null."))

(defclass account ()
  ((id         :col-type string  :reader   account-id)
   (title      :col-type string  :accessor title      :initarg :title)
   (debit-p    :col-type boolean :accessor debit-p    :initarg :debit-p)
   (parent-id  :col-type string  :accessor parent-id  :initarg :parent-id)
   (chequing-p :col-type boolean :accessor chequing-p :initarg :chequing-p))
  (:metaclass dao-class)
  (:keys id)
  (:default-initargs :title nil :chequing-p nil))

(defmethod account-id ((title (eql :null)))
  :null)

(define-alternate-key-readers account ((title string)) account-id)



;;; COMPANIES & CONTACTS

(defclass company ()
  ((id                  :col-type integer    :reader   company-id)
   (title               :col-type string     :accessor title                :initarg :title)
   (occupation          :col-type string     :accessor occupation           :initarg :occupation)
   (tof-id              :col-type integer    :accessor tof-id               :initarg :tof-id)
   (tin                 :col-type (string 9) :accessor tin                  :initarg :tin)
   (address             :col-type string     :accessor address              :initarg :address)
   (city-id             :col-type integer    :accessor city-id              :initarg :city-id)
   (pobox               :col-type integer    :accessor pobox                :initarg :pobox)
   (zipcode             :col-type integer    :accessor zipcode              :initarg :zipcode)
   (notes               :col-type string     :accessor notes                :initarg :notes)
   (revenues-account-id :col-type integer    :accessor revenues-account-id
                        :initarg :revenues-account-id)
   (expenses-account-id :col-type integer    :accessor expenses-account-id
                        :initarg :expenses-account-id)
   (immediate-tx-only-p :col-type boolean    :accessor immediate-tx-only-p
                        :initarg :immediate-tx-only-p))
  (:metaclass dao-class)
  (:keys id))

(defmethod company-id ((title (eql :null)))
  :null)

(define-alternate-key-readers company ((title string)) company-id)

(defclass contact ()
  ((id         :col-type integer :reader   contact-id)
   (company-id :col-type integer :accessor company-id :initarg :company-id)
   (tag        :col-type string  :accessor tag        :initarg :tag)
   (phone      :col-type string  :accessor phone      :initarg :phone)
   (rank       :col-type integer :accessor rank       :initarg :rank))
  (:metaclass dao-class)
  (:keys id))



;;; PROJECTS & BILLS

(defclass project-state ()
  ((state-id    :col-type string :reader   state-id)
   (description :col-type string :accessor description :initarg :description))
  (:metaclass dao-class)
  (:keys state-id))

(defclass project-stran ()
  ((id            :col-type integer :reader   project-stran-id)
   (title         :col-type string  :accessor title            :initarg :title)
   (from-state-id :col-type string  :accessor from-state-id    :initarg :from-state-id)
   (to-state-id   :col-type string  :accessor to-state-id      :initarg :to-state-id)
   (temtx-id      :col-type integer :accessor temtx-id         :initarg :temtx-id))
  (:metaclass dao-class)
  (:keys id))

(defclass project-event ()
  ((id               :col-type integer :reader id)
   (project-id       :col-type integer :accessor project-id       :initarg :project-id)
   (project-stran-id :col-type integer :accessor project-stran-id :initarg :project-stran-id)
   (tx-id            :col-type integer :accessor tx-id            :initarg :tx-id))
  (:metaclass dao-class)
  (:keys id))

(defclass project ()
  ((id          :col-type integer       :reader   project-id)
   (company-id  :col-type integer       :accessor company-id  :initarg :company-id)
   (description :col-type string        :accessor description :initarg :description)
   (price       :col-type (numeric 9 2) :accessor price       :initarg :price)
   (quote-date  :col-type timestamp     :accessor quote-date  :initarg :quote-date)
   (start-date  :col-type timestamp     :accessor start-date  :initarg :start-date)
   (end-date    :col-type timestamp     :accessor end-date    :initarg :end-date)
   (state-id    :col-type string        :accessor state-id    :initarg :state-id)
   (vat         :col-type (numeric 9 2) :accessor vat         :initarg :vat)
   (notes       :col-type string        :accessor notes       :initarg :notes))
  (:metaclass dao-class)
  (:keys id))

(defclass bill ()
  ((id         :col-type integer       :reader   bill-id)
   (project-id :col-type integer       :accessor project-id :initarg :project-id)
   (tag        :col-type string        :accessor tag        :initarg :tag)
   (amount     :col-type (numeric 9 2) :accessor amount     :initarg :amount)
   (rank       :col-type integer       :accessor rank       :initarg :rank))
  (:metaclass dao-class)
  (:keys id))



;;; CHEQUES

(defclass cheque-state ()
  ((id          :col-type string :reader   id)
   (description :col-type string :accessor description :initarg :description))
  (:metaclass dao-class)
  (:keys id))

(defclass cheque-stran ()
  ((id            :col-type integer :reader   cheque-stran-id)
   (title         :col-type string  :accessor title         :initarg :title)
   (from-state-id :col-type string  :accessor from-state-id :initarg :from-state-id)
   (to-state-id   :col-type string  :accessor to-state-id   :initarg :to-state-id)
   (customer-p    :col-type boolean :accessor customer-p    :initarg :customer-p)
   (temtx-id      :col-type integer :accessor temtx-id      :initarg :temtx-id))
  (:metaclass dao-class)
  (:keys id))

(defclass cheque-event ()
  ((id              :col-type integer   :reader   cheque-event-id)
   (tstamp          :col-type timestamp :accessor tstamp          :initarg :tstamp)
   (cheque-id       :col-type integer   :accessor cheque-id       :initarg :cheque-id)
   (tx-id           :col-type integer   :accessor tx-id           :initarg :tx-id)
   (cheque-stran-id :col-type integer   :accessor cheque-stran-id :initarg :cheque-stran-id))
  (:metaclass dao-class)
  (:keys id))

(defclass cheque ()
  ((id           :col-type integer       :reader   cheque-id)
   (bank-id      :col-type string        :accessor bank-id      :initarg :bank-id)
   (company-id   :col-type integer       :accessor company-id   :initarg :company-id)
   (due-date     :col-type timestamp     :accessor due-date     :initarg :due-date)
   (amount       :col-type (numeric 9 2) :accessor amount       :initarg :amount)
   (customer-p   :col-type boolean       :accessor customer-p   :initarg :customer-p)
   (state-id     :col-type (string 32)   :accessor state-id     :initarg :state-id)
   (serial       :col-type (string 16)   :accessor serial       :initarg :serial)
   (old-state-id :accessor old-state-id  :initarg  :old-state-id)) ;; not in the database
  (:metaclass dao-class)
  (:keys id))

(defmethod insert-dao :around ((dao cheque))
  (let* ((cheque-stran (select-dao-unique 'cheque-stran
                           (:and (:= 'from-state-id "nil")
                                 (:= 'customer-p (customer-p dao)))))
         (temtx (select-dao-unique 'temtx
                    (:= 'id (temtx-id cheque-stran))))
         (new-tx (make-instance 'tx
                                :tx-date (today)
                                :description (title cheque-stran)
                                :company-id (company-id dao)
                                :amount (amount dao)
                                :credit-account-id (credit-account-id temtx)
                                :debit-account-id (debit-account-id temtx))))
    (with-transaction ()
      (call-next-method)
      (insert-dao new-tx)
      (insert-dao (make-instance 'cheque-event
                                 :tstamp (now)
                                 :cheque-id (cheque-id dao)
                                 :cheque-stran-id (cheque-stran-id cheque-stran)
                                 :tx-id (tx-id new-tx)))
      dao)))

(defmethod update-dao :around ((cheque-dao cheque))
  (with-transaction ()
    ;; In case of state change, insert a corresponding tx and cheque-event
    (when-let (cheque-stran (select-dao-unique 'cheque-stran
                                (:and (:= 'customer-p (customer-p cheque-dao))
                                      (:= 'from-state-id (old-state-id cheque-dao))
                                      (:= 'to-state-id (state-id cheque-dao)))))
      (let* ((temtx (select-dao-unique 'temtx
                        (:= 'id (temtx-id cheque-stran))))
             (new-tx (make-instance 'tx
                                    :tx-date (today)
                                    :description (title cheque-stran)
                                    :company-id (company-id cheque-dao)
                                    :amount (amount cheque-dao)
                                    :credit-account-id (credit-account-id temtx)
                                    :debit-account-id (debit-account-id temtx))))
        (insert-dao new-tx)
        (insert-dao (make-instance 'cheque-event
                                   :tstamp (now)
                                   :cheque-id (cheque-id cheque-dao)
                                   :cheque-stran-id (cheque-stran-id cheque-stran)
                                   :tx-id (tx-id new-tx)))))
    ;; In any case, update cheque's data
    (call-next-method)
    ;; Also update the corresponding tx's data
    (let* ((cheque-event-daos (select-dao 'cheque-event (:= 'cheque-id (cheque-id cheque-dao))))
           (tx-daos (mapcar (compose (lambda (tx-id) (get-dao 'tx tx-id))
                                     #'tx-id)
                            cheque-event-daos)))
      (mapc (lambda (tx-dao)
              (setf (amount tx-dao) (amount cheque-dao)
                    (company-id tx-dao) (company-id cheque-dao))
              (update-dao tx-dao))
            tx-daos))))

(defmethod delete-dao :around ((dao cheque))
  (let* ((cheque-event-daos (select-dao 'cheque-event (:= 'cheque-id (cheque-id dao))))
         (tx-daos (mapcar (compose (lambda (tx-id) (get-dao 'tx tx-id))
                                   #'tx-id)
                          cheque-event-daos)))
    (with-transaction ()
      (mapc #'delete-dao cheque-event-daos)
      (mapc #'delete-dao tx-daos)
      (call-next-method))))



;;; TRANSACTIONS & TEMPLATES

(defclass tx ()
  ((id                :col-type integer       :reader   tx-id)
   (tx-date           :col-type timestamp     :accessor tx-date           :initarg :tx-date)
   (description       :col-type string        :accessor description       :initarg :description)
   (debit-account-id  :col-type integer       :accessor debit-account-id  :initarg :debit-account-id)
   (credit-account-id :col-type integer       :accessor credit-account-id :initarg :credit-account-id)
   (company-id        :col-type integer       :accessor company-id        :initarg :company-id)
   (amount            :col-type (numeric 9 2) :accessor amount            :initarg :amount)
   (temtx-id          :col-type integer       :reader   temtx-id))
  (:metaclass dao-class)
  (:keys id))

(defclass temtx ()
  ((id                :col-type integer :reader   temtx-id)
   (title             :col-type string  :accessor title             :initarg :title)
   (debit-account-id  :col-type integer :accessor debit-account-id  :initarg :debit-account-id)
   (credit-account-id :col-type integer :accessor credit-account-id :initarg :credit-account-id)
   (customer-p        :col-type boolean :accessor customer-p        :initarg :customer-p)
   (sign              :col-type integer :accessor sign              :initarg :sign)
   (propagated-p      :col-type boolean :accessor propagated-p      :initarg :propagated-p)
   (lib-p             :col-type boolean :accessor lib-p             :initarg :lib-p))
  (:metaclass dao-class)
  (:keys id))

(define-alternate-key-readers temtx ((title string)) temtx-id)

(defmethod temtx-id ((title (eql :null)))
  :null)



;;; EXTENSIONS OF SLOT ACCESSORS FOR BUILT-IN CLASSES

(defmethod customer-p ((role string))
  (string-equal role "customer"))
