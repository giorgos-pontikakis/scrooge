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
