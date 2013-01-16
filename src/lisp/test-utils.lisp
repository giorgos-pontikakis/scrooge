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
