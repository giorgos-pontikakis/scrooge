(in-package :scrooge)

#|
(with-db ()
  (account-balance
   (root (make-instance 'account-tree :debit-p t
                                      :op :catalogue))))
|#

;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass account-tx-family (family-mixin)
  ()
  (:default-initargs
   :parameter-groups '(:system (account-id)
                       :payload ()
                       :filter (since until))))

(defclass account-tx-page (auth-dynamic-page account-tx-family)
  ())



;;; ------------------------------------------------------------
;;; Account sums
;;; ------------------------------------------------------------

(defun account-sums-sql (account-id direction)
  (with-db ()
    (let ((column (ecase direction
                    (debit 'debit-account-id)
                    (credit 'credit-account-id))))
      (query (sql-compile `(:select (coalesce (sum tx.amount) 0)
                            :from tx
                            :where (:= ,column ,account-id)))
             :single))))

(defun account-sums (account-node)
  (let ((debits (account-sums-sql (key account-node) 'debit))
        (credits (account-sums-sql (key account-node) 'credit))
        (sign (if (debit-p (collection account-node)) 1 -1)))
    (values (* sign (- debits credits))
            (float debits)
            (float credits))))



;;; ------------------------------------------------------------
;;; Account tree with balance info
;;; ------------------------------------------------------------

;;; tree

(defclass balance-account-tree (account-tree)
  ()
  (:default-initargs :item-class 'balance-account-node))

(defclass balance-account-node (account-node)
  ((record-class :allocation :class :initform 'account)
   (cumul-balance :accessor cumul-balance
                  :initform nil)
   (balance :accessor balance
            :initform nil)
   (debits :accessor debits
           :initform nil)
   (credits :accessor credits
            :initform nil)))

(defmethod initialize-instance :after ((tree balance-account-tree) &key)
  (set-balance tree))

(defgeneric set-balance (balance-account-tree-or-node)
  (:documentation "Recursively transverse the account tree or subtree
  and set the balance, debits and credits slots"))

(defmethod set-balance ((tree balance-account-tree))
  (set-balance (root tree))
  tree)

(defmethod set-balance ((node balance-account-node))
  (multiple-value-bind (balance debits credits) (account-sums node)
    (if (children node)
        (progn
          (mapc #'set-balance (children node))
          (setf (cumul-balance node) (+ balance (reduce #'+ (mapcar #'cumul-balance (children node))))
                (balance node) balance
                (debits node) debits
                (credits node) credits)
          node)
        (progn
          (setf (cumul-balance node) balance)
          (setf (balance node) balance)
          (setf (debits node) debits)
          (setf (credits node) credits)
          node))))

(defmethod actions ((tree balance-account-tree) &key)
  (declare (ignore tree))
  (actions-menu nil))


;;; nodes

(defmethod selector ((node balance-account-node) selected-p)
  (let ((account-id (key node)))
    (html ()
      (:a :href (if selected-p
                    (account)
                    (account :account-id account-id))
        (selector-img selected-p)))))

(defmethod payload ((node balance-account-node) enabled-p)
  (macrolet ((mon (x)
               `(fmt "~,2F" ,x)))
    (html ()
      (:a :href (account/tx :account-id (key node))
        (str (getf (record node) :title)))
      (:span :class (conc "balance-details "
                          (if (non-negative-real-p (cumul-balance node)) "pos" "neg"))
        " [" (mon (cumul-balance node)) "]")
      (:ul
        (:li :class "debits-credits"
          "Balance: " (mon (balance node)) " | Debits: " (mon (debits node)) " | Credits: " (mon (credits node)))))))

(defmethod controls ((node balance-account-node) controls-p)
  (declare (ignore node))
  (list nil nil))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage account-tx-page account ("account")
    ((account-id integer chk-account-id))
  (with-view-page
    (with-document ()
      (:head
        (:title "Λογαριασμοί")
        (main-headers))
      (:body
        (:div :id "container" :class "container_12"
          (header)
          (main-navbar 'account)
          (loop for debit-p in '(t nil)
                for div-id in '("debit-accounts" "credit-accounts")
                for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί")
                for account-tree = (make-instance 'balance-account-tree
                                                  :op :catalogue
                                                  :selected-key (val account-id)
                                                  :debit-p debit-p)
                do (htm
                    (:div :class "grid_6"
                      (:div :id div-id :class "window"
                        (:div :class "title" (str window-title))
                        (actions account-tree)
                        (display account-tree :hide-root-p t)))))
          (footer))))))


;;; ----------------------------------------------------------------------
;;; account-tx table
;;; ----------------------------------------------------------------------

;;; table

(defclass account-tx-table (tx-table)
  ((header-labels :initform '("" "Ημερομηνία" "Περιγραφή" "Εταιρία" "Χρέωση" "Πίστωση"))
   (paginator :initform (make-instance 'account-tx-paginator
                                       :delta 1000
                                       :id "tx-paginator"
                                       :css-class "paginator"))
   (account-id :accessor account-id
               :initarg :account-id))
  (:default-initargs :item-class 'account-tx-row
                     :id "account-tx-table"))

(defmethod get-records ((table account-tx-table))
  (let* ((account-id (account-id table))
         (since (getf (filter table) :since))
         (until (getf (filter table) :until))
         (base-query `(:select tx.id
                               tx.tx-date
                               tx.description
                               (:as company.title company)
                               (:as company.id company-id)
                               debit-account-id
                               credit-account-id
                               amount
                       :from tx
                       :left-join company
                       :on (:= company.id tx.company-id)))
         (where nil)
         (where-base `(:or (:= ,account-id tx.debit-account-id)
                           (:= ,account-id tx.credit-account-id))))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since tx-date) where))
    (when (and until (not (eql until :null)))
      (push `(:<= tx-date ,until) where))
    (let ((sql `(:order-by (,@base-query :where (:and ,where-base ,@where))
                           (:desc tx-date) company description)))
      (query (sql-compile sql)
             :plists))))


;;; row

(defclass account-tx-row (scrooge-row/plist)
  ())

(defmethod selector ((row account-tx-row) selected-p)
  (declare (ignore row selected-p))
  (list nil nil))

(defmethod payload ((row account-tx-row) enabled-p)
  (let* ((record (record row))
         (account-id (account-id (collection row)))
         (record-plus (append record
                              (if (eql account-id (getf record :debit-account-id))
                                  (list :debit-amount (getf record :amount)
                                        :credit-amount :null)
                                  (list :debit-amount :null
                                        :credit-amount (getf record :amount))))))
    (list* (make-instance 'textbox
                          :name 'tx-date
                          :value (getf record-plus :tx-date)
                          :disabled (not enabled-p))
           (make-instance 'textbox
                          :name 'description
                          :value (getf record-plus :description)
                          :disabled (not enabled-p)
                          :href (tx :tx-id (getf record :id)))
           (make-instance 'textbox
                          :name 'company
                          :value (getf record-plus :company)
                          :disabled (not enabled-p)
                          :href (company/details :company-id (getf record :company-id)))
           (mapcar (lambda (name)
                     (make-instance 'textbox
                                    :name name
                                    :value (fmt-amount (getf record-plus (make-keyword name)))
                                    :disabled (not enabled-p)))
                   '(debit-amount credit-amount)))))

(defmethod controls ((row account-tx-row) enabled-p)
  (declare (ignore row enabled-p))
  (list nil nil))


;;; paginator

(defclass account-tx-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg account-tx-paginator) start)
  (let ((table (table pg)))
    (apply #'account/tx :account-id (account-id table) :start start (filter table))))



;;; ----------------------------------------------------------------------
;;; Account pages
;;; ----------------------------------------------------------------------

(defpage account-tx-page account/tx ("account/tx")
    ((account-id integer chk-account-id t)
     (start      integer))
  (with-view-page
    (let ((account-title (with-db ()
                           (title (get-dao 'account (val account-id)))))
          (tx-table (make-instance 'account-tx-table
                                   :op :catalogue
                                   :account-id (val account-id)
                                   :filter (params->filter)
                                   :start-index (val start))))
      (with-document ()
        (:head
          (:title "Λογαριασμοί » Συναλλαγές")
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header)
            (main-navbar 'account)
            (top-actions-area
             (make-instance 'scrooge-menu
                            :spec (make-menu-spec
                                   `(:catalogue ,(account :account-id (val account-id))))
                            :css-class "hmenu"
                            :disabled nil)
             nil)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" (str account-title))
                (display tx-table)))))))))
