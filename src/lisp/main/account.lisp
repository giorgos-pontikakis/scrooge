(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; tx-ro (read only) table
;;; ----------------------------------------------------------------------

(defclass account-tx-table (scrooge-table)
  ((header-labels  :initform '("" "Ημερομηνία" "Περιγραφή" "Εταιρία" "Χρέωση" "Πίστωση"))
   (paginator      :initform (make-instance 'scrooge-paginator
                                            :id "tx-paginator"
                                            :css-class "paginator grid_9 alpha"
                                            :urlfn #'account/details))
   (op :initform :read))
  (:default-initargs :item-class 'account-tx-row))

(defmethod get-records ((table account-tx-table))
  (let ((account-id (filter table)))
    (query (:select 'tx.id 'tx.tx-date 'tx.description (:as 'company.title 'company)
                    'debit-acc-id 'credit-acc-id 'amount
                    :from 'tx
                    :left-join 'company
                    :on (:= 'company.id 'tx.company-id)
                    :where (:or (:= account-id 'tx.debit-acc-id)
                                (:= account-id 'tx.credit-acc-id)))
           :plists)))


(defclass account-tx-row (scrooge-row/plist)
  ())

(defmethod selector ((row account-tx-row) selected-p)
  (declare (ignore row selected-p))
  (list nil nil))

(defmethod payload ((row account-tx-row) enabled-p)
  (let* ((record (record row))
         (account-id (filter (collection row)))
         (record-plus (append record
                              (if (eql account-id (getf record :debit-acc-id))
                                  (list :debit-amount (getf record :amount)
                                        :credit-amount :null)
                                  (list :debit-amount :null
                                        :credit-amount (getf record :amount))))))
    (mapcar (lambda (name)
              (make-instance 'textbox
                             :name name
                             :value (getf record-plus (make-keyword name))
                             :disabled (not enabled-p)))
            '(tx-date description company debit-amount credit-amount))))

(defmethod controls ((row account-tx-row) enabled-p)
  (declare (ignore row enabled-p))
  (list nil nil))



;;; ----------------------------------------------------------------------
;;; account-ro menu
;;; ----------------------------------------------------------------------

(defun account-ro-menu (id &optional disabled)
  (menu `((overview ,(account/overview :id id) "Σύνοψη")
          (details  ,(account/details :id id)  "Λεπτομέρειες")
          (print    ,(account/print :id id)    "Εκτύπωση"))
        :css-class "hnavbar actions grid_6 alpha"
        :disabled disabled))




;;; ----------------------------------------------------------------------
;;; Account pages
;;; ----------------------------------------------------------------------

(defpage dynamic-page account/overview ("account")
    ((id integer chk-account-id))
  (with-auth ("configuration")
    (no-cache)
    (with-document ()
      (:head
       (:title "Λογαριασμοί » Σύνοψη")
       (main-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header)
             (main-navbar 'account)
             (iter
               (for flag in (list t nil))
               (for div-id in '("debit-accounts" "credit-accounts"))
               (for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
               (for account-tree = (make-instance 'account-ro-tree
                                                  :debit-p flag))
               (htm
                (:div :class "grid_6"
                      (:div :id div-id :class "window"
                            (:div :class "title" (str window-title))
                            (account-ro-menu (val id)
                                             (if (and (val id) (eql flag (debit-p (val id))))
                                                 '(:overview)
                                                 '(:overview :details :print)))
                            (display account-tree :selected-id (val id)))))))))))


(defpage dynamic-page account/details ("account/details")
    ((id    integer chk-account-id)
     (start integer))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((account-title (with-db ()
                               (title (get-dao 'account (val id)))))
              (tx-table (make-instance 'account-tx-table
                                       :filter (val id)
                                       :start-index (val start))))
          (with-document ()
            (:head
             (:title "Λογαριασμοί » Λεπτομέρειες")
             (main-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header)
                   (main-navbar 'account)
                   (:div :class "grid_9"
                         (:div :class "window"
                               (:div :class "title"
                                     (str (conc "Ανάλυση Λογαριασμού: "
                                                account-title)))
                               (display tx-table)))))))
        (see-other (notfound)))))

(defpage dynamic-page account/print ("account/print")
    ((id integer chk-account-id))
  (with-auth ("configuration")
    (no-cache)
    (with-db ()
      (let ((account-title (title (get-dao 'account (val id))))
            (account-tx (query (:select 'tx.id 'tx-date 'description
                                        (:as 'debit-account.title 'debit-account-title)
                                        (:as 'credit-account.title 'credit-account-title)
                                        'amount
                                        :from 'tx
                                        :inner-join (:as 'account 'debit-account)
                                        :on (:= 'debit-account.id 'debit-acc-id)
                                        :inner-join (:as 'account 'credit-account)
                                        :on (:= 'credit-account.id 'credit-acc-id)
                                        :where (:or (:= id 'debit-account.id)
                                                    (:= id 'credit-account.id)))
                               :plists)))
        (with-document ()
          (:head
           (:title "Λογαριασμοί » Λεπτομέρειες")
           (main-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header)
                 (main-navbar 'account)
                 (:div :class "grid_9"
                       (:div :class "window"
                             (:div :class "title" (conc "Ανάλυση Λογαριασμού: "
                                                        account-title)
                                   (str account-tx)))))))))))