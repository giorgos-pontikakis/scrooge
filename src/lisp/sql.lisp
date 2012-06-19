(in-package :scrooge)


;;;  Customer

(defun customer-debits ()
  `(:in tx.credit-acc-id (:set ,@*revenues-accounts*)))

(defun customer-cash-credits ()
  `(:= tx.debit-acc-id ,*cash-acc-id*))

(defun customer-contra-credits ()
  `(:in tx.debit-acc-id (:set ,@*revenues-accounts*)))

(defun customer-cheque-credits ()
  ;; subquery receives cheque-event.cheque-id from main query
  `(:and (:= tx.debit-acc-id ,*cheque-receivable-acc-id*)
         (:not (:exists (:select 1
                         :from (:as tx tx2)
                         :inner-join (:as cheque-event cheque-event2)
                         :on (:= cheque-event2.tx-id tx2.id)
                         :where (:and
                                 (:= cheque-event2.cheque-id
                                     cheque-event.cheque-id)
                                 (:= tx2.credit-acc-id
                                     ,*cheque-receivable-acc-id*)))))))

;;;  Supplier

(defun supplier-credits ()
  `(:in tx.debit-acc-id (:set ,@*expense-accounts*)))

(defun supplier-cash-debits ()
  `(:= tx.credit-acc-id ,*cash-acc-id*))

(defun supplier-contra-debits ()
  `(:in tx.credit-acc-id (:set ,@*expense-accounts*)))

(defun supplier-cheque-debits ()
  ;; subquery receives cheque-event.cheque-id from main query
  `(:and (:= tx.credit-acc-id ,*cheque-payable-acc-id*)
         (:not (:exists (:select 1
                         :from (:as tx tx2)
                         :inner-join (:as cheque-event cheque-event2)
                         :on (:= cheque-event2.tx-id tx2.id)
                         :where (:and
                                 (:= cheque-event2.cheque-id
                                     cheque-event.cheque-id)
                                 (:= tx2.debit-acc-id
                                     ,*cheque-payable-acc-id*)))))))

(defun company-debits (company-id issuers &optional since until)
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
    (when (member :customer issuers)
      (push (customer-debits) where-tx))
    (when (member :supplier issuers)
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

(defun company-credits (company-id issuers &optional since until)
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
    (when (member :customer issuers)
      (push (customer-cash-credits) where-tx)
      (push (customer-contra-credits) where-tx)
      (push (customer-cheque-credits) where-tx))
    (when (member :supplier issuers)
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

(defun company-debits/credits (company-id issuers since until &key reverse-p)
  (flet ((get-tx-date (row)
           (getf row :tx-date)))
    (let* ((sorted (stable-sort (nconc (company-debits company-id issuers)
                                       (company-credits company-id issuers))
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