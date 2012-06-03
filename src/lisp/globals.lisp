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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (with-db ()
    ;; accounts
    (defparameter *accounts*
      (query (:select '* :from 'account) :plists))

    ;; roots
    (defparameter *debit-accounts-root*
      (select-dao-unique 'account (:and (:is-null 'parent-id)
                                        (:= 'debit-p t))))
    (defparameter *credit-accounts-root*
      (select-dao-unique 'account (:and (:is-null 'parent-id)
                                        (:= 'debit-p nil))))
    ;; cash
    (defparameter *cash-acc-id*
      (account-id (get-dao 'account-role "cash-account")))
    (defparameter *revenues-root-acc-id*
      (account-id (get-dao 'account-role "revenues-root-account")))
    (defparameter *expenses-root-acc-id*
      (account-id (get-dao 'account-role "expenses-root-account")))
    ;; invoices
    (defparameter *invoice-receivable-acc-id*
      (account-id (get-dao 'account-role "invoice-receivable-account")))
    (defparameter *invoice-payable-acc-id*
      (account-id (get-dao 'account-role "invoice-payable-account")))
    ;; cheques
    (defparameter *cheque-receivable-acc-id*
      (account-id (get-dao 'account-role "cheque-receivable-account")))
    (defparameter *cheque-payable-acc-id*
      (account-id  (get-dao 'account-role "cheque-payable-account")))))


(defun subaccounts (account-id)
  (flet ((get-children (records id)
           (remove-if-not (lambda (rec)
                            (eql (getf rec :parent-id) id))
                          records)))
    (dfs (lambda (head)
           (get-children *accounts*
                         (getf head :id)))
         (find account-id *accounts* :key (lambda (row)
                                            (getf row :id))))))

(defun subaccount-ids (account-id)
  (mapcar (lambda (plist)
            (getf plist :id))
          (subaccounts account-id)))

(with-db ()
  ;; account sets
  (defparameter *expense-accounts*
    (subaccount-ids *expenses-root-acc-id*))
  (defparameter *revenues-accounts*
    (subaccount-ids *revenues-root-acc-id*)))
