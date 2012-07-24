(in-package :scrooge)


;;; HTML

(eval-when (:compile-toplevel :load-toplevel)
  (setf (doctype) :xhtml)
  (setf (indent-mode) nil))


;;; VARIOUS LABELS

(defparameter *action-labels* '((:catalogue . "Κατάλογος")
                                (:create    . "Δημιουργία")
                                (:details   . "Λεπτομέρειες")
                                (:update    . "Επεξεργασία")
                                (:delete    . "Διαγραφή")
                                (:print     . "Εκτύπωση")))

(defparameter *project-state-ids* (lists->alist
                                (with-db ()
                                  (query (:select 'id 'description
                                                  :from 'project-state)))))

(defparameter *cheque-states*
  (lists->alist
   (with-db ()
     (query (:select 'id 'description
                     :from 'cheque-state)))))

(defparameter *default-project-state-id* "quoted")
(defparameter *default-cheque-state* "pending")
(defparameter *company-tx-significant-amount* 1.0)


;;; debit/credit account roots
(with-db ()
  (defparameter *debit-accounts-root-id*
    (select-dao-unique 'account (:and (:is-null 'parent-id)
                                      (:= 'debit-p t))))
  (defparameter *credit-accounts-root-id*
    (select-dao-unique 'account (:and (:is-null 'parent-id)
                                      (:= 'debit-p nil)))))



;;; ACCOUNT SETS

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *accounts* nil)
  (defparameter *expense-accounts* nil)
  (defparameter *revenue-accounts* nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((set-accounts ()
           (with-db ()
             (query (:select '* :from 'account) :plists)))
         (set-expense-accounts ()
           (subtree-record-ids *accounts*
                               (account-id 'expenses-root-account)))
         (set-revenue-accounts ()
           (subtree-record-ids *accounts*
                               (account-id 'revenues-root-account))))
    (defun update-account-globals ()
      (setf *accounts* (set-accounts))
      (setf *expense-accounts* (set-expense-accounts))
      (setf *revenue-accounts* (set-revenue-accounts)))))

;;; initialization of account sets
(update-account-globals)
