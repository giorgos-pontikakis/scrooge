(in-package :scrooge)

#|
(with-db ()
  (account-balance
   (root (make-instance 'account-tree :debit-p t
                                      :op :catalogue))))
|#



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
        (sign (if (debit-p (collection account-node)) +1.0 -1.0)))
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
          (setf (cumul-balance node) (+ balance (reduce #'+ (mapcar #'balance (children node))))
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

(defmethod actions ((tree account-tree) &key)
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
  (html ()
    (:strong (str (getf (record node) :title)))
    (:ul
      (:li "Cumulative Balance = " (str (cumul-balance node)) " | Balance = " (str (balance node)))
      (:li "Debits = " (str (debits node)) " | Credits = " (str (credits node))))))

(defmethod controls ((node account-node) controls-p)
  (declare (ignore node))
  (list nil nil))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage account-page account ("account")
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


;; ;;; ----------------------------------------------------------------------
;; ;;; tx-ro (read only) table
;; ;;; ----------------------------------------------------------------------

;; (defclass account-tx-table (scrooge-table)
;;   ((header-labels  :initform '("" "Ημερομηνία" "Περιγραφή" "Εταιρία" "Χρέωση" "Πίστωση"))
;;    (paginator      :initform (make-instance 'scrooge-paginator
;;                                             :id "tx-paginator"
;;                                             :css-class "paginator grid_9 alpha"))
;;    (op :accessor op :initform :catalogue))
;;   (:default-initargs :item-class 'account-tx-row))

;; (defmethod get-records ((table account-tx-table))
;;   (let ((account-id (filter table)))
;;     (query (:select 'tx.id 'tx.tx-date 'tx.description (:as 'company.title 'company)
;;                     'debit-account-id 'credit-account-id 'amount
;;             :from 'tx
;;             :left-join 'company
;;             :on (:= 'company.id 'tx.company-id)
;;             :where (:or (:= account-id 'tx.debit-account-id)
;;                         (:= account-id 'tx.credit-account-id)))
;;            :plists)))


;; (defclass account-tx-row (scrooge-row/plist)
;;   ())

;; (defmethod selector ((row account-tx-row) selected-p)
;;   (declare (ignore row selected-p))
;;   (list nil nil))

;; (defmethod payload ((row account-tx-row) enabled-p)
;;   (let* ((record (record row))
;;          (account-id (filter (collection row)))
;;          (record-plus (append record
;;                               (if (eql account-id (getf record :debit-account-id))
;;                                   (list :debit-amount (getf record :amount)
;;                                         :credit-amount :null)
;;                                   (list :debit-amount :null
;;                                         :credit-amount (getf record :amount))))))
;;     (mapcar (lambda (name)
;;               (make-instance 'textbox
;;                              :name name
;;                              :value (getf record-plus (make-keyword name))
;;                              :disabled (not enabled-p)))
;;             '(tx-date description company debit-amount credit-amount))))

;; (defmethod controls ((row account-tx-row) enabled-p)
;;   (declare (ignore row enabled-p))
;;   (list nil nil))



;; ;;; ----------------------------------------------------------------------
;; ;;; account-ro menu
;; ;;; ----------------------------------------------------------------------

;; (defun account-ro-menu (id &optional disabled)
;;   (menu `((overview ,(account/overview :id id) "Σύνοψη")
;;           (details  ,(account/details :id id)  "Λεπτομέρειες")
;;           (print    ,(account/print :id id)    "Εκτύπωση"))
;;         :css-class "hnavbar actions grid_6 alpha"
;;         :disabled disabled))

;; (defmethod actions ((tree account-tree/ro) &key)
;;   (actions-menu (make-menu-spec spec)
;;                 (if account-id)))


;; ;;; ----------------------------------------------------------------------
;; ;;; Account pages
;; ;;; ----------------------------------------------------------------------

;; (defpage dynamic-page account/overview ("account")
;;     ((id integer chk-account-id))
;;   (with-view-page
;;     (with-document ()
;;       (:head
;;         (:title "Λογαριασμοί » Σύνοψη")
;;         (main-headers))
;;       (:body
;;         (:div :id "container" :class "container_12"
;;           (header)
;;           (main-navbar 'account)
;;           (iter
;;               (for debit-p in (list tq nil))
;;                 (for div-id in '("debit-accounts" "credit-accounts"))
;;                 (for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
;;                 (for account-tree = (make-instance 'account-ro-tree
;;                                                    :op :catalogue
;;                                                    :selected-key (val account-id)
;;                                                    :debit-p debit-p))
;;                 (htm
;;                  (:div :class "grid_6"
;;                    (:div :id div-id :class "window"
;;                      (:div :class "title" (str window-title))
;;                      (account-ro-menu (val id)
;;                                       (if (and (val id) (eql flag (debit-p (val id))))
;;                                           '(:overview)
;;                                           '(:overview :details :print)))
;;                      (display account-tree :selected-id (val id)))))))))))


;; (defpage dynamic-page account/details ("account/details")
;;     ((id    integer chk-account-id)
;;      (start integer))
;;   (with-view-page
;;     (if (validp id)
;;         (let ((account-title (with-db ()
;;                                (title (get-dao 'account (val id)))))
;;               (tx-table (make-instance 'account-tx-table
;;                                        :filter (val id)
;;                                        :start-index (val start))))
;;           (with-document ()
;;             (:head
;;               (:title "Λογαριασμοί » Λεπτομέρειες")
;;               (main-headers))
;;             (:body
;;               (:div :id "container" :class "container_12"
;;                 (header)
;;                 (main-navbar 'account)
;;                 (:div :class "grid_9"
;;                   (:div :class "window"
;;                     (:div :class "title"
;;                       (str (conc "Ανάλυση Λογαριασμού: "
;;                                  account-title)))
;;                     (display tx-table)))))))
;;         (see-other (notfound)))))

;; (defpage dynamic-page account/print ("account/print")
;;     ((id integer chk-account-id))
;;   (with-view-page
;;     (let ((account-title (title (get-dao 'account (val id))))
;;           (account-tx (query (:select 'tx.id 'tx-date 'description
;;                                       (:as 'debit-account.title 'debit-account-title)
;;                                       (:as 'credit-account.title 'credit-account-title)
;;                                       'amount
;;                                       :from 'tx
;;                                       :inner-join (:as 'account 'debit-account)
;;                                       :on (:= 'debit-account.id 'debit-account-id)
;;                                       :inner-join (:as 'account 'credit-account)
;;                                       :on (:= 'credit-account.id 'credit-account-id)
;;                                       :where (:or (:= id 'debit-account.id)
;;                                                   (:= id 'credit-account.id)))
;;                              :plists)))
;;       (with-document ()
;;         (:head
;;           (:title "Λογαριασμοί » Λεπτομέρειες")
;;           (main-headers))
;;         (:body
;;           (:div :id "container" :class "container_12"
;;             (header)
;;             (main-navbar 'account)
;;             (:div :class "grid_9"
;;               (:div :class "window"
;;                 (:div :class "title" (conc "Ανάλυση Λογαριασμού: "
;;                                            account-title)
;;                   (str account-tx)))
;;               (print-pages-footer))))))))
