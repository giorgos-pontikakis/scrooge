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
	(zerop val)
	(plusp val))))

(defun positive-nonnull-p (val)
  (with-db
    (and (not (eq :null val))
	 (or (zerop val)
	     (plusp val)))))

;;; --- Config tables --------------------

(define-existence-validator tof-exists-p        tof     title)
(define-existence-validator city-exists-p       city    title)

(defun valid-tin-p (val)
  (or (eq :null val)
      (= (mod (iter (for m in '(256 128 64 32 16 8 4 2)) 
		    (for i in-string val) 
		    (sum (* m (parse-integer (string i)))))
	      11)
	 (parse-integer (subseq val 8)))))

(defun valid-tof-p (val)
  (or (eq :null val)
      (tof-exists-p val)))

(defun valid-city-p (val)
  (or (eq :null val)
      (city-exists-p val)))

;;; --- Companies --------------------

(define-existence-validator company-exists-p    company title)
(define-existence-validator company-id-exists-p tof     id)

(defun valid-company-p (val)
  (or (eq :null val)
      (company-exists-p val)))

(defun valid-company-id-p (val)
  (and (positive-nonnull-p val)
       (company-id-exists-p val)))


;;; --- Accounts --------------------

(define-existence-validator account-id-exists-p account id)
(define-existence-validator account-exists-p    account title)

(defun valid-account-id-p (val)
  (and (positive-p val)
       (account-id-exists-p val)))




;;; --- Transactions --------------------

(define-existence-validator tx-id-exists-p      tx      id)

(defun valid-tx-id-p (val)
  (and (positive-p val)
       (tx-id-exists-p val)))

;;; --- Transaction Types --------------------

(define-existence-validator tx-type-id-exists-p tx-type id)
(define-existence-validator tx-type-exists-p    tx-type title)

(defun valid-tx-type-id-p (val)
  (and (positive-p val)
       (tx-type-id-exists-p val)))

(defun valid-tx-type-p (val)
  (or (eq :null val)
      (tx-type-exists-p val)))

