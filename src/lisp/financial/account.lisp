(in-package :scrooge)


(defclass account-overview-node (account-node)
  ())

(defmethod cells ((node account-overview-node) &key)
  (let* ((id (key node))
         (record (record node)))
    (list :selector (make-instance 'selector-cell
                                   :states (list :on (account/overview)
                                                 :off (account/overview :id id)))
          :payload (make-instance 'textbox-cell
                                  :name 'title
                                  :value (getf record :title)))))



;;; ----------------------------------------------------------------------
;;; account-overview menu
;;; ----------------------------------------------------------------------

(defun account-overview-menu (id &optional disabled-items)
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
                                                  :node-class 'account-overview-node
                                                  :op 'catalogue
                                                  :filter flag))
               (htm
                (:div :id div-id :class "window grid_6"
                      (:div :class "title" (str window-title))
                      (account-overview-menu (val id)
                                             (if (and (val id) (eql flag (debit-p (val id))))
                                                 '(overview)
                                                 '(overview details print)))
                      (display account-tree :selected-id (val* id))))))))))

(defun account-transactions (id)
  (with-db ()
    (query (:select 'tx.id 'tx-date 'description
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


(define-dynamic-page account/details ("financial/account/details")
    ((id integer chk-acc-id))
  (with-auth ("configuration")
    (no-cache)
    (let ((account-title (with-db ()
                           (title (get-dao 'account (val id)))))
          (account-tx (account-transactions (val id))))
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
                     (str account-tx))))))))

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