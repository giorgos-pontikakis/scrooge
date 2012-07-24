(in-package :scrooge)



;;;  CUSTOMER DEBITS - CREDITS

(defun customer-debits ()
  `(:in tx.credit-acc-id (:set ,@*revenue-accounts*)))

(defun customer-cash-credits ()
  `(:= tx.debit-acc-id ,(account-id 'cash-account)))

(defun customer-contra-credits ()
  `(:in tx.debit-acc-id (:set ,@*revenue-accounts*)))

(defun customer-cheque-credits ()
  ;; subquery receives cheque-event.cheque-id from main query
  `(:and (:= tx.debit-acc-id ,(account-id 'cheque-receivable-account))
         (:not (:exists (:select 1
                         :from (:as tx tx2)
                         :inner-join (:as cheque-event cheque-event2)
                         :on (:= cheque-event2.tx-id tx2.id)
                         :where (:and
                                 (:= cheque-event2.cheque-id
                                     cheque-event.cheque-id)
                                 (:= tx2.credit-acc-id
                                     ,(account-id 'cheque-receivable-account))))))))



;;;  SUPPLIER CREDITS - DEBITS

(defun supplier-credits ()
  `(:in tx.debit-acc-id (:set ,@*expense-accounts*)))

(defun supplier-cash-debits ()
  `(:= tx.credit-acc-id ,(account-id 'cash-account)))

(defun supplier-contra-debits ()
  `(:in tx.credit-acc-id (:set ,@*expense-accounts*)))

(defun supplier-cheque-debits ()
  ;; subquery receives cheque-event.cheque-id from main query
  `(:and (:= tx.credit-acc-id ,(account-id 'cheque-payable-account))
         (:not (:exists (:select 1
                         :from (:as tx tx2)
                         :inner-join (:as cheque-event cheque-event2)
                         :on (:= cheque-event2.tx-id tx2.id)
                         :where (:and
                                 (:= cheque-event2.cheque-id
                                     cheque-event.cheque-id)
                                 (:= tx2.debit-acc-id
                                     ,(account-id 'cheque-payable-account))))))))



;;; COMPANY DEBITS - CREDITS

(defun company-debits (company-id roles &optional since until)
  (let ((base-query '(:select tx-date (:as tx.id id) tx.description
                      (:as tx.amount debit-amount) cheque.due-date cheque.state-id
                      :from tx
                      :left-join cheque-event
                      :on (:= cheque-event.tx-id tx.id)
                      :left-join cheque
                      :on (:= cheque.id cheque-event.cheque-id)))
        (where-base `(:= tx.company-id ,company-id))
        (where-tx '())
        (where-dates nil))
    (when (member :customer roles)
      (push (customer-debits) where-tx))
    (when (member :supplier roles)
      (push (supplier-cash-debits) where-tx)
      (push (supplier-contra-debits) where-tx)
      (push (supplier-cheque-debits) where-tx))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since tx-date) where-dates))
    (when (and until (not (eql until :null)))
      (push `(:<= tx-date ,until) where-dates))
    (let ((sql `(:order-by (,@base-query
                            :where (:and ,where-base
                                         (:or ,@where-tx)
                                         ,@where-dates))
                           'tx-date)))
      (query (sql-compile sql)
             :plists))))

(defun company-credits (company-id roles &optional since until)
  (let ((base-query '(:select tx-date (:as tx.id id) tx.description
                      (:as tx.amount credit-amount) cheque.due-date cheque.state-id
                      :from tx
                      :left-join cheque-event
                      :on (:= cheque-event.tx-id tx.id)
                      :left-join cheque
                      :on (:= cheque.id cheque-event.cheque-id)))
        (where-base `(:= tx.company-id ,company-id))
        (where-tx `())
        (where-dates nil))
    (when (member :customer roles)
      (push (customer-cash-credits) where-tx)
      (push (customer-contra-credits) where-tx)
      (push (customer-cheque-credits) where-tx))
    (when (member :supplier roles)
      (push (supplier-credits) where-tx))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since tx-date) where-dates))
    (when (and until (not (eql until :null)))
      (push `(:<= tx-date ,until) where-dates))
    (let ((sql `(:order-by (,@base-query
                            :where (:and ,where-base
                                         (:or ,@where-tx)
                                         ,@where-dates))
                           'tx-date)))
      (query (sql-compile sql)
             :plists))))

(defun company-debits/credits (company-id roles since until &key reverse-p)
  (flet ((get-tx-date (row)
           (getf row :tx-date))
         (get-tx-key (row)
           (getf row :id)))
    (let* ((company-debits (company-debits company-id roles))
           (company-credits (company-credits company-id roles))
           (merged (dolist (d company-debits (nconc company-debits company-credits))
                     (let ((credit-amount (dolist (c company-credits)
                                            (when (eql (get-tx-key c) (get-tx-key d))
                                              (let ((credit-amount (getf c :credit-amount)))
                                                (setf company-credits (delete c company-credits))
                                                (return credit-amount))))))
                       (when credit-amount
                         (setf d (nconc d (list :credit-amount credit-amount)))))))
           (sorted (stable-sort merged
                                #'local-time:timestamp<
                                :key #'get-tx-date))
           (truncated (remove-if (lambda (row)
                                   (or (and since
                                            (not (eql since :null))
                                            (timestamp< (get-tx-date row) since))
                                       (and until
                                            (not (eql until :null))
                                            (timestamp> (get-tx-date row) until))))
                                 sorted)))
      ;; calculate sums
      (let ((total 0)
            (debit-sum 0)
            (credit-sum 0))
        (dolist (row sorted)
          (let* ((debit (getf row :debit-amount 0))
                 (credit (getf row :credit-amount 0))
                 (delta (- debit credit)))
            (setf total (+ total delta))
            (setf debit-sum (+ debit-sum debit))
            (setf credit-sum (+ credit-sum credit))
            (when (cheque-row-p row)
              (setf (getf row :description)
                    (concatenate 'string
                                 (getf row :description)
                                 " (Λήξη: "
                                 (lisp->html (getf row :due-date))
                                 ")")))
            (nconc row (list :total total))))
        (values (if reverse-p
                    (nreverse truncated)
                    truncated)
                debit-sum credit-sum total)))))

(defun cheque-row-p (row)
  (let ((due-date (getf row :due-date)))
    (if (eql due-date :null)
        nil
        due-date)))



;;; ACCOUNTS PER DIRECTION

(defun incoming-p (direction)
  (string-equal direction "incoming"))

(defun revenues/expenses-root (direction)
  (if (incoming-p direction)
      (account-id 'revenues-root-account)
      (account-id 'expenses-root-account)))

(defun receivable/payable-root (direction)
  (if (incoming-p direction)
      (account-id 'receivable-root-account)
      (account-id 'payable-root-account)))

(defun revenues/expenses-set (direction)
  (if (incoming-p direction)
      *revenue-accounts*
      *expense-accounts*))
