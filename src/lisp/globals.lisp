(in-package :scrooge)


;;; ------------------------------------------------------------
;;; Globals
;;; ------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (setf (doctype) :xhtml)
  (setf (indent-mode) nil))

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


(with-db ()

  ;; debit/credit account roots
  (defparameter *debit-accounts-root-id*
    (select-dao-unique 'account (:and (:is-null 'parent-id)
                                      (:= 'debit-p t))))
  (defparameter *credit-accounts-root-id*
    (select-dao-unique 'account (:and (:is-null 'parent-id)
                                      (:= 'debit-p nil))))

  ;; subaccounts and subaccount-ids
  (let ((accounts (query (:select '* :from 'account) :plists)))
    (defun subaccounts (account-id)
      (flet ((get-children (records id)
               (remove-if-not (lambda (rec)
                                (eql (getf rec :parent-id) id))
                              records)))
        (dfs (lambda (head)
               (get-children accounts
                             (getf head :id)))
             (find account-id accounts :key (lambda (row)
                                              (getf row :id))))))
    (defun subaccount-ids (account-id)
      (mapcar (lambda (plist)
                (getf plist :id))
              (subaccounts account-id)))

    ;; account sets
    (defparameter *expense-accounts*
      (subaccount-ids (account-id 'expenses-root-account)))
    (defparameter *revenues-accounts*
      (subaccount-ids (account-id 'revenues-root-account)))))
