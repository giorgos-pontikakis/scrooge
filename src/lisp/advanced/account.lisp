(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass account-tx-family (family-mixin)
  ()
  (:default-initargs
   :parameter-groups '(:system (account-id tx-id start)
                       :payload ()
                       :filter (account-id search since until))))

(defclass account-tx-page (auth-dynamic-page account-tx-family)
  ())



;;; ------------------------------------------------------------
;;; Account sums
;;; ------------------------------------------------------------

(defun account-sums-sql (account-id direction since until)
  (with-db ()
    (let* ((column (ecase direction
                     (debit 'debit-account-id)
                     (credit 'credit-account-id)))
           (sql `(:select (coalesce (sum tx.amount) 0)
                  :from tx
                  :where (:and (:= ,column ,account-id)
                               ,(if since
                                    `(:<= ,since tx-date)
                                    t)
                               ,(if until
                                    `(:<= tx-date ,until)
                                    t)))))
      (query (sql-compile sql)
             :single))))

(defun account-sums (account-node since until)
  (let ((debits (account-sums-sql (key account-node) 'debit since  until))
        (credits (account-sums-sql (key account-node) 'credit since until))
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
  (let ((filter (filter (collection node))))
    (multiple-value-bind (balance debits credits) (account-sums node
                                                                (getf filter :since)
                                                                (getf filter :until))
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
            node)))))


(defmethod actions ((tree balance-account-tree) &key)
  (declare (ignore tree))
  (actions-menu nil))

;; (defmethod filters ((tree balance-account-tree))
;;   (let ((role (role tbl))
;;         (filter (filter tbl)))
;;     (top-actions-area
;;      (filter-navbar `((snapshot ,(apply #'cash "customer" filter) "Στιγμιότυπο")
;;                       (cumulative ,(apply #'cash "supplier" filter) "Αθροιστικά"))
;;                     :active role
;;                     :id "role-navbar")
;;      (datebox (lambda (&rest args)
;;                 (apply #'cash role args))
;;               filter))))

;;; nodes

(defmethod selector ((node balance-account-node) selected-p)
  (let ((filter (filter (collection node)))
        (account-id (key node)))
    (html ()
      (:a :href (if selected-p
                    (apply #'account filter)
                    (apply #'account :account-id account-id filter))
        (selector-img selected-p)))))

(defmethod payload ((node balance-account-node) enabled-p)
  (macrolet ((mon (x)
               `(fmt "~,2F" ,x)))
    (html ()
      (:a :href (account/tx :account-id (key node) :until (getf (filter (collection node)) :until))
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
    ((account-id integer chk-account-id)
     (since      date)
     (until      date))
  (with-view-page
    (let ((filter (params->filter)))
      (with-document ()
        (:head
          (:title "Λογαριασμοί")
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'advanced)
            (advanced-navbar 'account)
            (top-actions-area nil
                              (html ()
                                (:div :id "datebox" :class "inline-form filter-navbar"
                                  (with-form (account :account-id (val account-id))
                                    (:p
                                      (label 'since "Ημερομηνία: " :id "until")
                                      (input-text 'until :value (getf filter :until)
                                                         :css-class "datepicker")
                                      (:button :type "submit"
                                        (img "tick.png"))
                                      (:a :class "cancel"
                                        :href (account :account-id (val account-id))
                                        (img "cross.png")))))))
            (loop for debit-p in '(t nil)
                  for div-id in '("debit-accounts" "credit-accounts")
                  for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί")
                  for account-tree = (make-instance 'balance-account-tree
                                                    :op :catalogue
                                                    :selected-key (val account-id)
                                                    :debit-p debit-p
                                                    :filter filter)
                  do (htm
                      (:div :class "grid_6"
                        (:div :id div-id :class "window"
                          (:div :class "title" (str window-title))
                          (actions account-tree)
                          (display account-tree :hide-root-p t)))))
            (footer)))))))



;;; ----------------------------------------------------------------------
;;; account-tx table
;;; ----------------------------------------------------------------------

;;; table

(defclass account-tx-table (tx-table)
  ((header-labels :initform '("" "Ημερομηνία" "Περιγραφή" "Εταιρία" "Χρέωση" "Πίστωση"))
   (paginator :initform (make-instance 'account-tx-paginator
                                       :id "tx-paginator"
                                       :css-class "paginator"))
   (account-id :accessor account-id
               :initarg :account-id))
  (:default-initargs :item-class 'account-tx-row
                     :id "account-tx-table"))

(defmethod get-records ((table account-tx-table))
  (let* ((account-id (account-id table))
         (search (getf (filter table) :search))
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
    (when search
      (push `(:or (:ilike tx.description ,(ilike search))
                  (:ilike company.title ,(ilike search)))
            where))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since tx-date) where))
    (when (and until (not (eql until :null)))
      (push `(:<= tx-date ,until) where))
    (let ((sql `(:order-by (,@base-query :where (:and ,where-base ,@where))
                           (:desc tx-date) company description)))
      (query (sql-compile sql)
             :plists))))

(defmethod filters ((tbl account-tx-table))
  (filter-area (datebox #'account/tx
                        (list* :account-id (account-id tbl) (filter tbl)))))


;;; row

(defclass account-tx-row (scrooge-row/plist)
  ())

(defmethod selector ((row account-tx-row) selected-p)
  (let* ((table (collection row))
         (tx-id (key row))
         (account-id (account-id table))
         (filter (filter table))
         (start (page-start (paginator table) (index row) (start-index table))))
    (html ()
      (:a :href (if selected-p
                    (apply #'account/tx :account-id account-id
                                        :start start filter)
                    (apply #'account/tx :account-id account-id
                                        :tx-id tx-id filter))
        (selector-img selected-p)))))

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
     (tx-id      integer chk-tx-id)
     (search     string)
     (since      date)
     (until      date)
     (start      integer))
  (with-view-page
    (let* ((account-title (with-db ()
                            (title (get-dao 'account (val account-id)))))
           (account-tx-table (make-instance 'account-tx-table
                                            :op :catalogue
                                            :account-id (val account-id)
                                            :selected-key (val tx-id)
                                            :filter (params->filter)
                                            :start-index (val start)))
           (debit-sum 0)
           (credit-sum 0)
           (total 0))
      (dolist (rec (records account-tx-table))
        (let ((amount (getf rec :amount)))
          (if (eql (getf rec :debit-account-id) (val account-id))
              (progn (incf debit-sum amount)
                     (incf total amount))
              (progn (incf credit-sum amount)
                     (decf total amount)))))
      (with-document ()
        (:head
          (:title (str (conc "Λογαριασμοί » Συναλλαγές: " account-title)))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'advanced)
            (advanced-navbar 'account)
            (top-actions-area
             (make-instance 'scrooge-menu
                            :spec (make-menu-spec
                                   `(:catalogue ,(account :account-id (val account-id)
                                                          :until (val until))))
                            :css-class "hmenu"
                            :disabled nil)
             (searchbox (family-url-fn 'account/tx)
                        (family-url-fn 'account/tx :system)
                        (family-params 'account/tx :filter)
                        "ac-company"))
            (filters account-tx-table)
            (:div :class "grid_12"
              (:div :id "account-tx-window" :class "window"
                (:div :class "title" (str account-title))
                (display account-tx-table)
                (:h4 "Σύνολο Χρεώσεων: " (fmt "~9,2F" debit-sum))
                (:h4 "Σύνολο Πιστώσεων: " (fmt "~9,2F" credit-sum))
                (:h4 "Γενικό Σύνολο: " (fmt "~9,2F" total))))))))))
