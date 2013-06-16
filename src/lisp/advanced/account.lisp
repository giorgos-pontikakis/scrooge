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
;;; Account tree with balance info
;;; ------------------------------------------------------------

;;; tree

(defclass balance-account-tree (account-tree)
  ()
  (:default-initargs :item-class 'balance-account-node))

(defclass balance-account-node (account-node)
  ((cumul-balance :accessor cumul-balance :initform nil)
   (balance       :accessor balance       :initform nil)
   (debits        :accessor debits        :initform nil)
   (credits       :accessor credits       :initform nil)))

(defmethod display ((tree balance-account-tree) &key payload hide-root-p)
  (set-balance tree)
  (call-next-method tree :payload payload :hide-root-p hide-root-p))

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
            (setf (cumul-balance node) (+ balance
                                          (reduce #'+
                                                  (mapcar #'cumul-balance
                                                          (children node))))
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
  (declare (ignore enabled-p))
  (html ()
    (:a :href (apply #'account/tx :account-id (key node) (filter (collection node)))
        (str (getf (record node) :title)))
    (:span :class (conc "balance-details "
                        (if (non-negative-real-p (cumul-balance node)) "pos" "neg"))
           " [" (str (fmt-amount (cumul-balance node))) "]")
    (:ul
     (:li :class "debits-credits"
          "Balance: " (str (fmt-amount (balance node)))
          " | Debits: " (str (fmt-amount (debits node)))
          " | Credits: " (str (fmt-amount (credits node)))))))

(defmethod controls ((node balance-account-node) controls-p)
  (declare (ignore node controls-p))
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
         (advanced-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'advanced)
               (navbar 'advanced 'account)
               (top-actions-area (datebox #'account filter) nil)
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
  ((account-id :accessor account-id :initarg :account-id))
  (:default-initargs :item-class 'account-tx-row
                     :id "account-tx-table"
                     :paginator (make-instance 'account-tx-paginator)
                     :header-labels '("" "Ημερομηνία" "Περιγραφή" "Εταιρία" "Χρέωση" "Πίστωση")))

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

(defmethod extra-info ((table account-tx-table))
  (let ((debit-sum 0)
        (credit-sum 0)
        (total 0))
    (dolist (rec (records table))
      (let ((amount (getf rec :amount)))
        (if (eql (getf rec :debit-account-id) (account-id table))
            (progn (incf debit-sum amount)
                   (incf total amount))
            (progn (incf credit-sum amount)
                   (decf total amount)))))
    (with-html
      (:div :class "window-footer"
            (:h4 "Σύνολο Χρεώσεων: " (str (fmt-amount debit-sum)))
            (:h4 "Σύνολο Πιστώσεων: " (str (fmt-amount credit-sum)))
            (:h4 "Γενικό Σύνολο: " (str (fmt-amount total)))))))


;;; row

(defclass account-tx-row (scrooge-row)
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
    (mapply (factory #'make-instance 'table-textbox :enabled enabled-p :record record-plus)
            `((:name tx-date)
              (:name description :href ,(tx :tx-id (getf record :id)))
              (:name company :href ,(company/details :company-id (getf record :company-id)))
              (:name debit-amount :format-fn ,#'fmt-amount)
              (:name credit-amount :format-fn ,#'fmt-amount)))))

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
                                            :start-index (val start))))
      (with-document ()
        (:head
         (:title (str (conc "Λογαριασμοί » Συναλλαγές: " account-title)))
         (advanced-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'advanced)
               (navbar 'advanced 'account)
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
                           (extra-info account-tx-table)))))))))
