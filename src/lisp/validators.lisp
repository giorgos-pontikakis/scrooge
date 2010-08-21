(in-package :scrooge)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-existence-validator (name table field)
    (with-gensyms (value)
      `(defun ,name (,value)
	 (with-db 
	   (query (:select 1 :from ',table :where (:= ',field ,value))))))))


;;; --- Helpers --------------------

(defun not-db-null-p (val)
  (not (eq :null val)))

(defun positive-p (val)
  (or (eq :null val)
      (and (integerp val)
	   (or (zerop val)
	       (plusp val)))))

(defun positive-nonnull-p (val)
  (and (not (eq :null val))
       (or (zerop val)
	   (plusp val))))


;;; --- Banks --------------------

(define-existence-validator bank-id-exists-p bank id)
(define-existence-validator bank-exists-p bank title)


;;; --- Taxation Offices --------------------

(define-existence-validator tof-id-exists-p tof id)
(define-existence-validator tof-exists-p tof title)


;;; --- Cities --------------------

(define-existence-validator city-id-exists-p city id)
(define-existence-validator city-exists-p city title)




;;; --- TIN --------------------

(defun valid-tin-p (val)
  (or (eq :null val)
      (and (= (length val) 9)
	   (= (mod (iter (for m in '(256 128 64 32 16 8 4 2)) 
			 (for i in-string val) 
			 (sum (* m (parse-integer (string i)))))
		   11)
	      (parse-integer (subseq val 8))))))


;;; --- Companies --------------------

(define-existence-validator company-exists-p    company title)
(define-existence-validator company-id-exists-p company id)

(defun valid-company-p (val)
  (company-exists-p val))

(defun valid-company-id-p (val) 
  (and (positive-nonnull-p val)
       (company-id-exists-p val)))


;;; --- Contacts --------------------

(define-existence-validator contact-id-exists-p contact id)

(defun valid-contact-id-p (val)
  (and (positive-nonnull-p val)
       (contact-id-exists-p val)))


;;; --- Accounts --------------------

(define-existence-validator acc-id-exists-p account id)
(define-existence-validator acc-exists-p    account title)

(defun valid-acc-id-p (val)
  (and (positive-nonnull-p val)
       (acc-id-exists-p val)))

(defun valid-parent-acc-id-p (val)
  (or (null val)
      (acc-id-exists-p val)))

(defun valid-acc-id-no-subaccounts-p (acc-id)
  (and (acc-id-exists-p acc-id)
       (null (get-subaccounts acc-id))))

(defun valid-debp-id-combo (id debp)
  (with-db
    (or (null id)
        (eql debp (debit-p (get-dao 'account id))))))


;;; --- Transactions --------------------

(define-existence-validator tx-id-exists-p tx id)

(defun valid-tx-id-p (val)
  (and (positive-nonnull-p val)
       (tx-id-exists-p val)))


;;; --- Transaction Templates --------------------

(define-existence-validator temtx-id-exists-p temtx id)
(define-existence-validator temtx-exists-p    temtx description)

(defun valid-temtx-id-p (val)
  (and (positive-p val)
       (temtx-id-exists-p val)))


;;; --- Cheques --------------------

(define-existence-validator cheque-id-exists-p cheque id)

(defun valid-cheque-id-p (val)
  (and (positive-nonnull-p val)
       (cheque-id-exists-p val)))

(defun valid-due-date-p (val)
  (not (eq :null val)))

(defun valid-cheque-status-p (val)
  (with-db
    (query (:select 1 :from 'cheque-status :where (:= 'status (if (symbolp val)
								  (string-downcase val)
								  val))))))


;;; --- Projects --------------------

(define-existence-validator valid-project-id-p project id)


;;; --- State transitions --------------------

(defun valid-stran-id-p (stran-id tbl)
  (with-db
    (query (:select 1
		    :from (symbolicate (string-upcase tbl) "-STRAN")
		    :where (:= 'id stran-id)))))

(defun valid-tbl-p (tbl)
  (with-db
    (query (:select 1
                    :from 'stran
                    :where (:= tbl 'id)))))

(defun valid-combo (table status)
  (with-db
    (let* ((status-table (symbolicate (string-upcase table) "-STATUS"))
	   (sql (sql-compile `(:select 1
				       :from ,status-table
				       :where (:= status ,status)))))
      (and (valid-tbl-p table)
	   (query sql :single)))))