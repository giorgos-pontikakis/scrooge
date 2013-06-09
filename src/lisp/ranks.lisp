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
  (loop for i in (higher-rank-daos dao)
        do (setf (rank i) (+ (rank i) delta))
           (update-dao i)))

(defgeneric higher-rank-daos (dao)
  (:documentation "Get all daos of db-table with rank greater or equal
  to the given daos rank."))

(defmethod higher-rank-daos ((dao bill))
  (select-dao 'bill
      (:and (:not (:= 'id (bill-id dao)))
            (:= 'project-id (project-id dao))
            (:>= 'rank (rank dao)))))

(defmethod higher-rank-daos ((dao contact))
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
