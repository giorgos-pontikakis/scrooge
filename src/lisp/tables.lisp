(in-package :scrooge)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-surrogate-key-readers (table (&rest surrogate-key-types) &rest slots)
    `(progn
       ,@(mapcar (lambda (slot)
                   `(defmethod ,slot ,surrogate-key-types
                      (with-db ()
                        (,slot (select-dao-unique
                                ',table
                                (:and ,@(mapcar (lambda (surrogate-key)
                                                  `(:= ',surrogate-key ,surrogate-key))
                                                (mapcar #'first surrogate-key-types))))))))
                 slots))))

(defgeneric get-record (type primary-key)
  (:documentation "Modeled after get-dao, but it should return a plist"))



;;; ------------------------------------------------------------
;;; Configuration
;;; ------------------------------------------------------------

(defclass usr ()
  ((username  :col-type (string 128) :accessor username  :initarg :username)
   (password  :col-type (string 128) :accessor password  :initarg :password)
   (authgroup :col-type (string 128) :accessor authgroup :initarg :authgroup))
  (:metaclass dao-class)
  (:keys username))



(defclass bank ()
  ((id    :col-type integer :reader   bank-id)
   (title :col-type string  :accessor title   :initarg :title))
  (:metaclass dao-class)
  (:keys id))

(define-surrogate-key-readers bank ((title string)) bank-id)

(defmethod bank-id ((title (eql :null)))
  :null)


(defclass tof ()
  ((id    :col-type integer :reader   tof-id)
   (title :col-type string  :accessor title  :initarg :title))
  (:metaclass dao-class)
  (:keys id))

(define-surrogate-key-readers tof ((title string)) tof-id)

(defmethod tof-id ((title (eql :null)))
  :null)


(defclass city ()
  ((id    :col-type integer :reader   city-id)
   (title :col-type string  :accessor title :initarg :title))
  (:metaclass dao-class)
  (:keys id))

(define-surrogate-key-readers city ((title string)) city-id)

(defmethod city-id ((title (eql :null)))
  :null)


(defclass account-role ()
  ((id          :col-type (string 32)  :reader   id)
   (description :col-type (string 100) :reader   description)
   (account-id  :col-type integer      :accessor account-id  :initarg :account-id))
  (:metaclass dao-class)
  (:keys id))

(defclass account ()
  ((id         :col-type string  :reader   account-id)
   (title      :col-type string  :accessor title      :initarg :title)
   (debit-p    :col-type boolean :accessor debit-p    :initarg :debit-p)
   (parent-id  :col-type string  :accessor parent-id  :initarg :parent-id)
   (chequing-p :col-type boolean :accessor chequing-p :initarg :chequing-p))
  (:metaclass dao-class)
  (:keys id))

(defmethod account-id ((title (eql :null)))
  :null)

(define-surrogate-key-readers account ((title string)) account-id)



;;; ------------------------------------------------------------
;;; Companies & Contacts
;;; ------------------------------------------------------------

(defclass company ()
  ((id         :col-type integer :reader   company-id)
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
  (:keys id))

(defmethod company-id ((title (eql :null)))
  :null)

(define-surrogate-key-readers company ((title string)) company-id)

(defclass contact ()
  ((id         :col-type integer :reader   contact-id)
   (company-id :col-type integer :accessor company-id :initarg :company-id)
   (tag        :col-type string  :accessor tag        :initarg :tag)
   (phone      :col-type string  :accessor phone      :initarg :phone)
   (rank       :col-type integer :accessor rank       :initarg :rank))
  (:metaclass dao-class)
  (:keys id))


;;; ------------------------------------------------------------
;;; Projects & Bills
;;; ------------------------------------------------------------

(defclass project-state ()
  ((state       :col-type string :reader   state)
   (description :col-type string :accessor description :initarg :description))
  (:metaclass dao-class)
  (:keys state))

(defclass project-stran ()
  ((id            :col-type integer :reader   project-stran-id)
   (title         :col-type string  :accessor title            :initarg :title)
   (from-state    :col-type string  :accessor from-state       :initarg :from-state)
   (to-state      :col-type string  :accessor to-state         :initarg :to-state)
   (debit-acc-id  :col-type integer :accessor debit-acc-id     :initarg :debit-acc-id)
   (credit-acc-id :col-type integer :accessor credit-acc-id    :initarg :credit-acc-id))
  (:metaclass dao-class)
  (:keys id))

(defclass project-event ()
  ((id               :accessor id               :initarg :id)
   (project-id       :accessor project-id       :initarg :project-id)
   (project-stran-id :accessor project-stran-id :initarg :project-stran-id)
   (tx-id            :accessor tx-id            :initarg :tx-id))
  (:metaclass dao-class)
  (:keys id))

(defclass project ()
  ((id          :col-type integer       :reader   project-id)
   (company-id  :col-type integer       :accessor company-id  :initarg :company-id)
   (description :col-type string        :accessor description :initarg :description)
   (location    :col-type string        :accessor location    :initarg :location)
   (price       :col-type (numeric 9 2) :accessor price       :initarg :price)
   (quote-date  :col-type date          :accessor quote-date  :initarg :quote-date)
   (start-date  :col-type date          :accessor start-date  :initarg :start-date)
   (end-date    :col-type date          :accessor end-date    :initarg :end-date)
   (state       :col-type string        :accessor state       :initarg :state)
   (vat         :col-type (numeric 9 2) :accessor vat         :initarg :vat)
   (notes       :col-type string        :accessor notes       :initarg :notes))
  (:metaclass dao-class)
  (:keys id))

(defclass bill ()
  ((id         :col-type integer :reader   bill-id)
   (project-id :col-type integer :accessor project-id :initarg :project-id)
   (tag        :col-type string  :accessor tag        :initarg :tag)
   (amount     :col-type string  :accessor amount     :initarg :amount)
   (rank       :col-type integer :accessor rank       :initarg :rank))
  (:metaclass dao-class)
  (:keys id))



;;; ------------------------------------------------------------
;;; Cheques
;;; ------------------------------------------------------------

(defclass cheque-state ()
  ((state       :col-type string :reader   state)
   (description :col-type string :accessor description :initarg :description))
  (:metaclass dao-class)
  (:keys state))

(defclass cheque-stran ()
  ((id            :col-type integer :reader   cheque-stran-id)
   (title         :col-type string  :accessor title           :initarg :title)
   (payable-p     :col-type boolean :accessor payable-p       :initarg :payable-p)
   (from-state    :col-type string  :accessor from-state      :initarg :from-state)
   (to-state      :col-type string  :accessor to-state        :initarg :to-state)
   (debit-acc-id  :col-type integer :accessor debit-acc-id    :initarg :debit-acc-id)
   (credit-acc-id :col-type integer :accessor credit-acc-id   :initarg :credit-acc-id))
  (:metaclass dao-class)
  (:keys id))

(defclass cheque-event ()
  ((id              :col-type integer :reader   id              :initarg :id)
   (event-date      :col-type date    :accessor event-date      :initarg :event-date)
   (cheque-id       :col-type integer :accessor cheque-id       :initarg :cheque-id)
   (cheque-stran-id :col-type integer :accessor cheque-stran-id :initarg :cheque-stran-id)
   (tx-id           :col-type integer :accessor tx-id           :initarg :tx-id))
  (:metaclass dao-class)
  (:keys id))

(defclass cheque ()
  ((id         :col-type integer       :reader   cheque-id)
   (bank-id    :col-type string        :accessor bank-id    :initarg :bank-id)
   (company-id :col-type integer       :accessor company-id :initarg :company-id)
   (due-date   :col-type date          :accessor due-date   :initarg :due-date)
   (amount     :col-type (numeric 9 2) :accessor amount     :initarg :amount)
   (state      :col-type string        :accessor state      :initarg :state)
   (payable-p  :col-type boolean       :accessor payable-p  :initarg :payable-p))
  (:metaclass dao-class)
  (:keys id))



;;; ------------------------------------------------------------
;;; Transactions & Templates
;;; ------------------------------------------------------------

(defclass tx ()
  ((id            :col-type integer       :reader   tx-id)
   (tx-date       :col-type date          :accessor tx-date       :initarg :tx-date)
   (description   :col-type string        :accessor description   :initarg :description)
   (debit-acc-id  :col-type string        :accessor debit-acc-id  :initarg :debit-acc-id)
   (credit-acc-id :col-type string        :accessor credit-acc-id :initarg :credit-acc-id)
   (company-id    :col-type integer       :accessor company-id    :initarg :company-id)
   (amount        :col-type (numeric 9 2) :accessor amount        :initarg :amount))
  (:metaclass dao-class)
  (:keys id))

(defclass temtx ()
  ((id            :col-type integer :reader   temtx-id)
   (title         :col-type string  :accessor title         :initarg :title)
   (debit-acc-id  :col-type string  :accessor debit-acc-id  :initarg :debit-acc-id)
   (credit-acc-id :col-type string  :accessor credit-acc-id :initarg :credit-acc-id))
  (:metaclass dao-class)
  (:keys id))

(define-surrogate-key-readers temtx ((title string)) temtx-id)



;;; ------------------------------------------------------------
;;; RANK UTILITIES
;;; ------------------------------------------------------------

;;; Max Rank

(defgeneric max-rank (dao)
  (:documentation
   "Find the maximum rank for a dao."))

(defmethod max-rank ((dao bill))
  (let ((ranks (query (:select 'rank
                       :from 'bill
                       :where (:= 'project-id (project-id dao)))
                      :column)))
    (if ranks (reduce #'max ranks) 0)))

(defmethod max-rank ((dao contact))
  (let ((ranks (query (:select 'rank
                       :from 'contact
                       :where (:= 'company-id (company-id dao)))
                      :column)))
    (if ranks (reduce #'max ranks) 0)))



;;; Shift ranks

(defun shift-higher-rank-daos (dao delta)
  "Increase by delta the rank of daos which have rank greater or equal
  to the reference dao"
  (iter (for i in (higher-rank-daos dao delta))
        (setf (rank i) (+ (rank i) delta))
        (update-dao i)))

(defgeneric higher-rank-daos (dao delta)
  (:documentation "Get all daos of db-table with rank greater or equal
  to the given daos rank."))

(defmethod higher-rank-daos ((dao bill) delta)
  (select-dao 'bill
              (:and (:not (:= 'id (bill-id dao)))
                    (:= 'project-id (project-id dao))
                    (:>= 'rank (rank dao)))))

(defmethod higher-rank-daos ((dao contact) delta)
  (select-dao 'contact (:and (:not (:= 'id (contact-id dao)))
                             (:= 'company-id (company-id dao))
                             (:>= 'rank (rank dao)))))



;;; Swap ranks

(defun swap-ranks (dao delta)
  "Swap ranks for a reference dao and a neighbour dao which has rank different by delta."
  (let* ((rank (rank dao))
         (other-rank (+ rank delta))
         (other-dao (dao-neighbour dao delta)))
    (unless (null other-dao)
      (setf (rank dao) other-rank)
      (setf (rank other-dao) rank)
      (with-transaction ()
        (update-dao dao)
        (update-dao other-dao)))))

(defgeneric dao-neighbour (dao delta)
  (:documentation
   "Given a dao, select another dao which has rank different by delta."))

(defmethod dao-neighbour ((dao bill) delta)
  (select-dao-unique 'bill (:and (:= 'project-id (project-id dao))
                                 (:= 'rank (+ (rank dao) delta)))))

(defmethod dao-neighbour ((dao contact) delta)
  (select-dao-unique 'contact (:and (:= 'company-id (company-id dao))
                                    (:= 'rank (+ (rank dao) delta)))))
