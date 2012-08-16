(in-package :scrooge)

(defun companies-without-tx-with-revenues/expenses-account ()
  (with-db ()
    (query (:select 'company.id 'company.title :from 'company
             :where (:and (:or (:not (:is-null 'revenues-account-id))
                               (:not (:is-null 'expenses-account-id)))
                          (:not (:exists (:select 'tx.id :from 'tx
                                           :where (:= 'company.id 'tx.company-id))))))
           :plists)))

(defun check-company-debits/credits ()
  (with-db ()
    (mapcar (lambda (i)
              (company-debits/credits (getf i :id) (list :customer :supplier) nil nil))
            (companies-without-tx-with-revenues/expenses-account))))



;;; DATABASE INTEGRITY CHECKS

(defun companies-with-tx-without-revenues/expenses-account ()
  (with-db ()
    (query (:select 'company.id 'company.title :from 'company
             :where (:and (:is-null 'revenues-account-id)
                          (:is-null 'expenses-account-id)
                          (:exists (:select 'tx.id :from 'tx
                                     :where (:= 'company.id 'tx.company-id)))))
           :plists)))

(defun companies-with-revenues-tx-without-revenues-account ()
  (with-db ()
    (let ((conflicts
            (query (:select 'company.id 'company.title :from 'company
                     :where
                     (:and (:is-null 'revenues-account-id)
                           (:exists (:select 'tx.id :from 'tx
                                      :where (:and (:= 'company.id 'tx.company-id)
                                                   (:in 'tx.credit-acc-id
                                                        (:set *revenue-accounts*)))))))
                   :plists)))
      (values conflicts
              (mapcar (lambda (company)
                        (query (:select
                                 'tx.description 'tx.amount
                                 'debit-account.title 'credit-account.title
                                 :from 'tx
                                 :inner-join (:as 'account 'debit-account)
                                 :on (:= 'tx.debit-acc-id 'debit-account.id)
                                 :inner-join (:as 'account 'credit-account)
                                 :on (:= 'tx.credit-acc-id 'credit-account.id)
                                 :where (:and (:= 'tx.company-id (getf company :id))
                                              (:in 'tx.credit-acc-id
                                                        (:set *revenue-accounts*))))))
                      conflicts)))))


(defun suppliers-with-customer-txs ()
  (with-db ()
    (query (:select 'tx.id 'company.title 'tx.description :from 'tx
             :inner-join 'company
             :on (:= 'tx.company-id 'company-id)
             :where (:and (:= 'company.id 'tx.company-id)
                          (:or (:in 'tx.credit-acc-id
                                    (:set *revenue-accounts*))
                               (:in 'tx.credit-acc-id
                                    (:set *receivable-accounts*)))
                          (:is-null 'company.revenues-account-id)))
           :plists)))


(defun customers-with-supplier-txs ()
  (with-db ()
    (query (:select 'tx.id 'company.title 'tx.description :from 'tx
             :inner-join 'company
             :on (:= 'tx.company-id 'company-id)
             :where (:and (:= 'company.id 'tx.company-id)
                          (:or (:= 'tx.credit-acc-id (account-id 'cash))
                               (:in 'tx.debit-acc-id (:set *payable-accounts*)))
                          (:is-null 'company.expenses-account-id)))
           :plists)))


(defun unknown-txs ()
  "Returns the number of transactions that have no corresponding temtx"
  (with-db ()
    (query (sql-compile `(:select (count *) :from (:as (:select * (find-temtx id) :from tx) tx-augmented)
                           :where (:is-null tx-augmented.find-temtx)))
           :single!)))

(assert (= (unknown-txs) 0))
