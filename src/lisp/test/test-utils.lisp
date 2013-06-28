(in-package :scrooge)

(defun account-companies (account-id)
  "Unique names of all companies which exhibit at least one
transaction in the account with the given account-id "
  (with-db ()
    (loop for i in (remove-duplicates (query (:select 'company.title
                                              :from 'company
                                              :left-join 'tx
                                              :on (:= 'tx.company-id 'company.id)
                                              :where (:or (:= 'tx.debit-account-id account-id)
                                                          (:= 'tx.credit-account-id account-id)))
                                             :column)
                                      :test #'string=)
          do (princ i)
             (terpri))))


(defun creditors-not-in-payables ()
  "Companies with credit balance which exhibit no credit transactions
in payables"
  (with-db ()
    (let ((sql `(:select company.title
                 :from company
                 :where (:and (:= -1 (sign (company-balance company.id)))
                              (:not (:in company.id
                                         (:select tx.company-id
                                          :from tx
                                          :where (:in tx.credit-account-id
                                                      (:union (:select ,@*payable-accounts*)
                                                              (:select (account-descendants ,@*payable-accounts*)))))))))))
      (query (sql-compile sql) :column))))

(defun debtors-not-in-receivables ()
  "Companies with debit balance which exhibit no debit transactions in
receivables"
  (with-db ()
    (let ((sql `(:select company.title
                 :from company
                 :where (:and (:= +1 (sign (company-balance company.id)))
                              (:not (:in company.id
                                         (:select tx.company-id
                                          :from tx
                                          :where (:in tx.debit-account-id
                                                      (:union (:select ,@*receivable-accounts*)
                                                              (:select (account-descendants ,@*receivable-accounts*)))))))))))
      (query (sql-compile sql) :column))))

(defun companies-with-non-default-expense/revenue-txs ()
  (with-db ()
    (let ((sql `(:select company.title
                 :distinct
                 :from company
                 :left-join tx
                 :on (:= company.id tx.company-id)
                 :where (:or (:and (:in tx.debit-account-id (:set ,@*expense-accounts*))
                                   (:not (:= tx.debit-account-id company.expenses-account-id)))
                             (:and (:in tx.credit-account-id (:set ,@*revenue-accounts*))
                                   (:not (:= company.revenues-account-id tx.credit-account-id)))))))
      (query (sql-compile sql) :column))))

(defun txs-with-wrong-temtx-id ()
  (with-db ()
    (query (:select '*
            :from 'tx
            :where (:not (:= (:get-temtx 'tx.debit-account-id 'tx.credit-account-id)
                             'temtx-id)))
           :plists)))


;;; TX INFO

;; (defun unhandled-transactions ()
;;   (with-db ()
;;     (let ((all-txs (query (:select '* :from 'tx) :plists)))
;;       (loop for tx in all-txs
;;             for temtx-id = (getf tx :temtx-id)
;;             unless (normally-handled-section-for-temtx temtx-id)
;;               collect (list (getf tx :id)
;;                             (getf tx :description)
;;                             (title (get-dao 'company (getf tx :company-id))))))))



;;; CORRECTIVE ACTIONS

(defun move-general-revenues-to-company-default-accounts (&key doit)
  (with-db ()
    (let ((txs (query (:select 'tx.id 'company-id 'tx.credit-account-id
                               'company.revenues-account-id 'tx.description
                       :from 'tx
                       :left-join 'company
                       :on (:= 'company.id 'tx.company-id)
                       :where (:= 'tx.credit-account-id 5))
                      :plists)))
      (if doit
          (loop for tx in txs
                do (ignore-errors (execute (:update 'tx
                                            :set 'credit-account-id (getf tx :revenues-account-id)
                                            :where (:= 'id (getf tx :id))))))
          txs))))


(defun move-expense-to-default-account-for-company (company &key doit)
  (with-db ()
    (let ((sql `(:select company-id tx.id
                         (:as debit-account.title debit-account-title)
                         (:as credit-account.title credit-account-title)
                         company.expenses-account-id tx.description
                         :from tx
                         :left-join company
                         :on (:= company.id tx.company-id)
                         :left-join (:as account debit-account)
                         :on (:= debit-account.id tx.debit-account-id)
                         :left-join (:as account credit-account)
                         :on (:= credit-account.id tx.credit-account-id)
                         :where (:and (:ilike company.title ,(ilike company))
                                      (:in tx.debit-account-id (:set ,@*expense-accounts*))
                                      (:not (:= tx.debit-account-id company.expenses-account-id))))))
      (let ((txs (query (sql-compile sql) :plists)))
        (if doit
            (loop for tx in txs
                  do (execute (:update 'tx
                               :set 'debit-account-id (getf tx :expenses-account-id)
                               :where (:= 'id (getf tx :id)))))
            txs)))))



;;; DEBUGGING CODE

(defun purge-company (company-id)
  (with-db ()
    ;; cheque-events
    (print (list 'cheque-events-purged
                 (execute (:delete-from 'cheque-event
                           :where (:in 'cheque-event.cheque-id
                                       (:select 'id :from 'cheque
                                        :where (:= company-id 'cheque.company-id) ))))))
    ;; cheques
    (print (list 'cheques-purged
                 (execute (:delete-from 'cheque
                           :where (:= company-id 'cheque.company-id)))))
    ;; txs
    (print (list 'txs-purged
                 (execute (:delete-from 'tx
                           :where (:= company-id 'tx.company-id)))))
    ;; contacts
    (print (list 'contacts-purged
                 (execute (:delete-from 'contact
                           :where (:= company-id 'contact.company-id)))))
    ;; bills
    (print (list 'bills-purged
                 (execute (:delete-from 'bill
                           :where (:in 'bill.project-id
                                       (:select 'id :from 'project
                                        :where (:= company-id 'project.company-id)))))))
    ;; projects
    (print (list 'projects-purged
                 (execute (:delete-from 'project
                           :where (:= company-id 'project.company-id)))))
    ;; company per se
    (print (execute (:delete-from 'company :where (:= 'company.id company-id))))))


(defun account-balance (account-id)
  (with-db ()
    (float (- (account-sums-sql account-id 'debit nil nil)
              (account-sums-sql account-id 'credit nil nil)))))


(defun companies-sum (subset)
  (with-db ()
    (let ((records (records (make-instance 'company-table :filter (list :subset subset)
                                                          :op :read))))
      (float (reduce #'+
                     (mapcar (getfer :balance 0)
                             records))))))

(defun sum-error ()
  (- (account-balance (account-id 'receivable-root-account))
     (companies-sum "debit")))
