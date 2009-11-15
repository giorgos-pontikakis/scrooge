(in-package :scrooge)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-pk-existence-validator (name table field)
    (with-gensyms (value)
      `(defun ,name (,value)
	 (and (positive-nonnull-p ,value)
	      (with-db
		(query (:select 1 :from ',table :where (:= ',field ,value)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-existence-validator (name table field)
    (with-gensyms (value)
      `(defun ,name (,value)
	 (with-db
	   (query (:select 1 :from ',table :where (:= ',field ,value))))))))


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

(define-pk-existence-validator valid-company-id-p company id)
(define-pk-existence-validator valid-tx-id-p      tx      id)

(define-existence-validator valid-company-title-p company title)
(define-existence-validator valid-tx-type-title-p tx-type title)


