(in-package :scrooge)


(defun unknown-txs ()
  (with-db ()
    (query (sql-compile
            '(:select tx.id debit-account.title credit-account.title :from tx
              :left-join (:as account debit-account)
              :on (:= debit-account.id tx.debit-acc-id)
              :left-join (:as account credit-account)
              :on (:= credit-account.id tx.credit-acc-id)
              :where
              (:not (:in tx.id
                     (:select tx.id :from tx
                       :inner-join temtx
                       :on (:and
                            (:in temtx.debit-acc-id = :any (:select debit-account.lineage))
                            (:in temtx.credit-acc-id = :any (:select credit-account.lineage))))))))
           :plists)))


;;; COMPANY BALANCE

(defun company-debits/credits-sql (company-id roles)
  (let (temtx-conditions)
    ;; roles
    (when (member :customer roles)
      (push '(:= temtx.customer-p t) temtx-conditions))
    (when (member :supplier roles)
      (push '(:= temtx.customer-p nil) temtx-conditions))
    ;; query -- uses SQL function: descendants
    `(:order-by (:select tx-date tx.id tx.description tx.amount
                  temtx.sign cheque.due-date cheque.state-id
                  :from tx
                  :left-join cheque-event
                  :on (:= cheque-event.tx-id tx.id)
                  :left-join cheque
                  :on (:= cheque.id cheque-event.cheque-id)
                  :left-join (:as account debit-account)
                  :on (:= debit-account.id tx.debit-acc-id)
                  :left-join (:as account credit-account)
                  :on (:= credit-account.id tx.credit-acc-id)
                  :inner-join temtx
                  :on (:and (:in temtx.debit-acc-id (:set (:[] debit-account.lineage 0)))
                            (:in temtx.credit-acc-id (:set (:[] credit-account.lineage 0)))
                            (:= tx.company-id ,company-id)))
                tx-date tx.description)))

(defun company-debits/credits-all (company-id roles)
  (flet ((amounts (row)
           (let ((sign (getf row :sign))
                 (amount (getf row :amount)))
             (cond ((= sign +1) (values amount nil amount))
                   ((= sign -1) (values nil amount (- amount)))
                   ((= sign 0) (values amount amount 0)))))
         (cheque-row-p (row)
           (not (eql (getf row :due-date) :null))))
    (with-db ()
      (let ((all-tx-rows (query (sql-compile (company-debits/credits-sql company-id roles)) :plists))
            (total 0)
            (debit-sum 0)
            (credit-sum 0))
        ;; Modify rows
        (dolist (row all-tx-rows)
          (multiple-value-bind (debit credit delta) (amounts row)
            ;; amounts
            (when delta
              (incf total delta))
            (when debit
              (incf debit-sum debit))
            (when credit
              (incf credit-sum credit))
            (nconc row (list :debit-amount debit
                             :credit-amount credit
                             :total total))
            ;; cheque descriptions
            (when (cheque-row-p row)
              (setf (getf row :description)
                    (concatenate 'string
                                 (getf row :description)
                                 " (Λήξη: "
                                 (lisp->html (getf row :due-date))
                                 ")")))))
        ;; Return results
        (values all-tx-rows debit-sum credit-sum total)))))


(defun company-debits/credits (company-id roles since until &key reverse-p)
  (flet ((get-tx-date (row)
           (getf row :tx-date)))
    ;; Truncate rows and maybe revers result
    (multiple-value-bind (all-tx-rows debit-sum credit-sum total)
        (company-debits/credits-all company-id roles)
      (let ((truncated (remove-if (lambda (row)
                                    (or (and since
                                             (not (eql since :null))
                                             (timestamp< (get-tx-date row) since))
                                        (and until
                                             (not (eql until :null))
                                             (timestamp> (get-tx-date row) until))))
                                  all-tx-rows)))
        (values (if reverse-p
                    (nreverse truncated)
                    truncated)
                debit-sum
                credit-sum
                total)))))



;;; ACCOUNTS PER ROLE

(defun customer-p (role)
  (string-equal role "customer"))

(defun revenues/expenses-root (role)
  (if (customer-p role)
      (account-id 'revenues-root-account)
      (account-id 'expenses-root-account)))

(defun receivable/payable-root (role)
  (if (customer-p role)
      (account-id 'receivable-root-account)
      (account-id 'payable-root-account)))

(defun revenues/expenses-set (role)
  (if (customer-p role)
      *revenue-accounts*
      *expense-accounts*))
