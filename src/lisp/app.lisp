(in-package :scrooge)



;;; RANK UTILITIES

;;; Max rank

(defgeneric max-rank (dao)
  (:documentation
   "Find the maximum rank for a dao."))

(defmethod max-rank ((dao bill))
  (let ((ranks (query (:select 'rank
                       :from 'bill
                       :where (:= 'project-id (project-id dao)))
                      :column)))
    (if ranks (reduce #'max ranks) 0)))

(defmethod max-rank ((dao contact))
  (let ((ranks (query (:select 'rank
                       :from 'contact
                       :where (:= 'company-id (company-id dao)))
                      :column)))
    (if ranks (reduce #'max ranks) 0)))


;;; Shift ranks

(defun shift-higher-rank-daos (dao delta)
  "Increase by delta the rank of daos which have rank greater or equal
  to the reference dao"
  (loop for i in (higher-rank-daos dao delta)
        do (setf (rank i) (+ (rank i) delta))
           (update-dao i)))

(defgeneric higher-rank-daos (dao delta)
  (:documentation "Get all daos of db-table with rank greater or equal
  to the given daos rank."))

(defmethod higher-rank-daos ((dao bill) delta)
  (select-dao 'bill
      (:and (:not (:= 'id (bill-id dao)))
            (:= 'project-id (project-id dao))
            (:>= 'rank (rank dao)))))

(defmethod higher-rank-daos ((dao contact) delta)
  (select-dao 'contact (:and (:not (:= 'id (contact-id dao)))
                             (:= 'company-id (company-id dao))
                             (:>= 'rank (rank dao)))))


;;; Swap ranks

(defun swap-ranks (dao delta)
  "Swap ranks for a reference dao and a neighbour dao which has rank different by delta."
  (let* ((rank (rank dao))
         (other-rank (+ rank delta))
         (other-dao (dao-neighbour dao delta)))
    (unless (null other-dao)
      (setf (rank dao) other-rank)
      (setf (rank other-dao) rank)
      (with-transaction ()
        (update-dao dao)
        (update-dao other-dao)))))

(defgeneric dao-neighbour (dao delta)
  (:documentation
   "Given a dao, select another dao which has rank different by delta."))

(defmethod dao-neighbour ((dao bill) delta)
  (select-dao-unique 'bill (:and (:= 'project-id (project-id dao))
                                 (:= 'rank (+ (rank dao) delta)))))

(defmethod dao-neighbour ((dao contact) delta)
  (select-dao-unique 'contact (:and (:= 'company-id (company-id dao))
                                    (:= 'rank (+ (rank dao) delta)))))



;;; COMPANY DEBITS/CREDITS

(defun company-debits/credits-sql (company-id roles)
  (let (temtx-conditions)
    ;; roles
    (when (member :customer roles)
      (push '(:= temtx.customer-p t) temtx-conditions))
    (when (member :supplier roles)
      (push '(:= temtx.customer-p nil) temtx-conditions))
    ;; query
    `(:order-by (:select tx-date tx.id tx.description tx.amount
                         temtx.sign cheque.due-date cheque.state-id
                         ;; the following are needed for the payload of company-tx-row
                         tx.temtx-id
                         temtx.lib-p (:as cheque.id cheque-id)
                         temtx.debit-account-id temtx.credit-account-id
                 :from tx
                 :left-join cheque-event
                 :on (:= cheque-event.tx-id tx.id)
                 :left-join cheque
                 :on (:= cheque.id cheque-event.cheque-id)
                 :left-join cheque-stran
                 :on (:= cheque-stran.id cheque-event.cheque-stran-id)
                 :inner-join temtx
                 :on (:= temtx.id tx.temtx-id)
                 :where (:and (:= tx.company-id ,company-id)
                              (:or ,@temtx-conditions)))
                tx-date tx.description)))

(defun company-debits/credits-all (company-id roles)
  (flet ((amounts (row)
           (let ((sign (getf row :sign))
                 (amount (getf row :amount)))
             (cond ((eql sign :null) (values nil nil 0))
                   ((eql sign +1) (values amount nil amount))
                   ((eql sign -1) (values nil amount (- amount)))
                   ((eql sign 0) (values amount amount 0)))))
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
    ;; Truncate rows and maybe revert results
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
