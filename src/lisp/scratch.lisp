(in-package :scrooge)


(defun asset-transfers ()
  (with-db ()
    (let ((sql `(:select * :from tx
                 :where (:and (:ilike description ,(ilike "Μεταφορά"))
                              (:= credit-account-id 58)))))
      (query (sql-compile sql) :plists))))

(defun liability-transfers ()
  (with-db ()
    (let ((sql `(:select * :from tx
                 :where (:and (:ilike description ,(ilike "Μεταφορά"))
                              (:= debit-account-id 58)))))
      (query (sql-compile sql) :plists))))

(defun correct-asset-transfers ()
  (with-db ()
    (let ((sql `(:select * :from tx
                 :where (:and (:ilike description ,(ilike "Μεταφορά"))
                              (:= debit-account-id 67)
                              (:or (:= credit-account-id 7)
                                   (:= credit-account-id 8))))))
      (loop for tx in (query (sql-compile sql) :plists)
            do (execute (:update 'tx
                         :set 'credit-account-id 58
                         :where (:= 'id (getf tx :id))))))))

(defun correct-expense-transfers ()
  (with-db ()
    (let ((sql `(:select * :from tx
                 :where (:and (:ilike description ,(ilike "Μεταφορά"))
                              (:= credit-account-id 68)
                              (:in debit-account-id (:set ,@*expense-accounts*))))))
      (loop for tx in (query (sql-compile sql) :plists)
            do (execute (:update 'tx
                         :set 'debit-account-id 58
                         :where (:= 'id (getf tx :id))))))))

(defun correct-transfer-dates ()
  (with-db ()
    (let ((sql `(:select * :from tx
                 :where (:or (:= debit-account-id 58)
                             (:= credit-account-id 58)))))
      (loop for tx in (query (sql-compile sql) :plists)
            do (execute (:update 'tx
                         :set 'tx-date (encode-timestamp 0 0 0 0 1 1 2012 :timezone +utc-zone+)
                         :where (:= 'id (getf tx :id))))))))
