(in-package :scrooge)

;;; ----------------------------------------------------------------------
;;; account-ro (read only) tree
;;; ----------------------------------------------------------------------

(defclass account-ro-tree (account-crud-tree)
  ((op :initform 'catalogue))
  (:default-initargs :item-class 'account-ro-node))

(defclass account-ro-node (account-crud-node)
  ())

(defmethod cells ((node account-ro-node) &key)
  (let* ((id (key node))
         (record (record node)))
    (list :selector (make-instance 'selector-cell
                                   :states (list :on (account/overview)
                                                 :off (account/overview :id id)))
          :payload (make-instance 'textbox-cell
                                  :name 'title
                                  :value (getf record :title)))))



;;; ----------------------------------------------------------------------
;;; tx-ro (read only) table
;;; ----------------------------------------------------------------------

(defclass account-tx-table (crud-table)
  ((header-labels :initform '("" "Ημερομηνία" "Περιγραφή" "Εταιρία" "Χρέωση" "Πίστωση"))
   (paginator     :initform (make-instance 'paginator
                                           :id "tx-paginator"
                                           :style "paginator grid_9 alpha"
                                           :delta 10
                                           :urlfn (lambda (filter start)
                                                    (declare (ignore filter))
                                                    (account/details :start start))))
   (op :initform 'catalogue))
  (:default-initargs :item-class 'account-tx-row))

(defmethod read-records ((table account-tx-table))
  (let ((account-id (filter table)))
    (with-db ()
      (query (:select 'tx.id 'tx-date 'description 'company.title
                      'debit-acc-id 'credit-acc-id 'amount
                      :from 'tx
                      :inner-join 'company
                      :on (:= 'company.id 'tx.company-id)
                      :where (:or (:= account-id 'debit-account.id)
                                  (:= account-id 'credit-account.id)))
             :plists))))


(defclass account-tx-row (crud-row)
  ())

(defmethod cells ((row account-tx-row) &key)
  (let* ((record (record row))
         (account-id (filter (table row)))
         (amounts (if (eql account-id (getf record :debit-acc-id))
                      (list (getf record :amount)
                            nil)
                      (list nil
                            (getf record :amount)))))
    (list :payload (mapcar (lambda (name)
                             (make-instance 'textbox-cell
                                            :name name
                                            :value (getf record (make-keyword name))))
                           (append '(date description company)
                                   amounts)))))



;;; ----------------------------------------------------------------------
;;; account-ro menu
;;; ----------------------------------------------------------------------

(defun account-ro-menu (id &optional disabled-items)
  (display (make-instance 'actions-menu
                          :style "hnavbar actions grid_6 alpha"
                          :spec `((overview ,(account/overview :id id) "Σύνοψη")
                                  (details  ,(account/details :id id)  "Λεπτομέρειες")
                                  (print    ,(account/print :id id)    "Εκτύπωση")))
           :disabled-items disabled-items))




;;; ----------------------------------------------------------------------
;;; Account pages
;;; ----------------------------------------------------------------------

(define-dynamic-page account/overview ("financial/account")
    ((id integer chk-acc-id))
  (with-auth ("configuration")
    (no-cache)
    (with-document ()
      (:head
       (:title "Λογαριασμοί > Σύνοψη")
       (financial-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'financial)
             (financial-navbar 'financial/account)
             (iter
               (for flag in (list t nil))
               (for div-id in '("debit-accounts" "credit-accounts"))
               (for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
               (for account-tree = (make-instance 'account-tree
                                                  :filter flag))
               (htm
                (:div :id div-id :class "window grid_6"
                      (:div :class "title" (str window-title))
                      (account-ro-menu (val id)
                                       (if (and (val id) (eql flag (debit-p (val id))))
                                           '(overview)
                                           '(overview details print)))
                      (display account-tree :selected-id (val* id))))))))))


(define-dynamic-page account/details ("financial/account/details")
    ((id    integer chk-acc-id)
     (start integer))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((account-title (with-db ()
                               (title (get-dao 'account (val id)))))
              (tx-table (make-instance 'tx-accounts-table
                                       :filter (val id))))
          (with-document ()
            (:head
             (:title "Λογαριασμοί > Λεπτομέρειες")
             (financial-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'financial)
                   (financial-navbar 'financial/account)
                   (:div :class "window grid_9"
                         (:div :class "title"
                               (str (conc "Ανάλυση Λογαριασμού: "
                                          account-title)))
                         (display tx-table
                                  :start (val* start)))))))
        (see-other (notfound)))))

(define-dynamic-page account/print ("financial/account/print")
    ((id integer chk-acc-id))
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
           (:title "Λογαριασμοί > Λεπτομέρειες")
           (financial-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'financial)
                 (financial-navbar 'financial/account)
                 (:div :class "window grid_9"
                       (:div :class "title" (conc "Ανάλυση Λογαριασμού: "
                                                  account-title)
                             (str account-tx))))))))))