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
  (with-db
    (or (eq :null val)
	(and (integerp val)
	     (or (zerop val)
		 (plusp val))))))

(defun positive-nonnull-p (val)
  (with-db
    (and (not (eq :null val))
	 (or (zerop val)
	     (plusp val)))))


;;; --- Config tables --------------------

(define-existence-validator tof-exists-p  tof  title)
(define-existence-validator city-exists-p city title)
(define-existence-validator bank-exists-p bank title)

(defun valid-tin-p (val)
  (or (eq :null val)
      (and (= (length val) 9)
	   (= (mod (iter (for m in '(256 128 64 32 16 8 4 2)) 
			 (for i in-string val) 
			 (sum (* m (parse-integer (string i)))))
		   11)
	      (parse-integer (subseq val 8))))))

(defun valid-tof-p (val)
  (or (eq :null val)
      (tof-exists-p val)))

(defun valid-bank-p (val)
  (bank-exists-p val))

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

(define-existence-validator account-id-exists-p account id)
(define-existence-validator account-exists-p    account title)

(defun valid-account-id-p (val)
  (and (positive-nonnull-p val)
       (account-id-exists-p val)))

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

(defun valid-fsm-id-p (fsm-id tbl)
  (with-db
    (query (:select 1
		    :from (symbolicate (string-upcase tbl) "-FSM")
		    :where (:= 'id fsm-id)))))

(defun valid-combo (table old-status new-status)
  (with-db
    (let* ((status-table (symbolicate (string-upcase table) "-STATUS"))
	   (sql-old (sql-compile `(:select 1
					   :from ,status-table
					   :where (or (:= status ,old-status)
						      (:is-null ,old-status)))))
	   (sql-new  (sql-compile `(:select 1
					    :from ,status-table
					    :where (:= status ,new-status)))))
      (and (member table (fsm-tables) :test #'string-equal)
	   (query sql-old :single)
	   (query sql-new :single)))))

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
;;; --- Accounts --------------------

(define-existence-validator acc-id-exists-p account id)


(defun valid-parent-acc-id-p (val)
  (or (eql val :null)
      (acc-id-exists-p val)))