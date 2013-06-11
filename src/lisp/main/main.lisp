(in-package :scrooge)


;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defmethod navbar ((section (eql 'main)) active)
  (declare (ignore section))
  (let ((spec  `((company ,(company)
                          "Εταιρίες")
                 (project ,(project :cstate "ongoing")
                          "Έργα")
                 (cash    ,(cash "customer")
                          "Μετρητά")
                 (cheque  ,(cheque "customer" :cstate "pending")
                          "Επιταγές")
                 (invoice ,(invoice "customer" "debit")
                          "Χρεωπιστώσεις")
                 (libtx   ,(libtx "customer")
                          "Βιβλιοθήκη"))))
    (with-html
        (:div :class "grid_12"
              (:div :class "section-navbar"
                    (obj 'navbar
                         :spec spec
                         :css-class "hnavbar"
                         :active active))))))


;;; app-section

(defun app-section (temtx-id)
  (with-db ()
    (let* ((temtx (get-dao 'temtx temtx-id))
           (db-id (debit-account-id temtx))
           (cr-id (credit-account-id temtx))
           (cash-id (account-id 'cash-account))
           (normal-section (cond
                             ;; cash
                             ((or (and (eql db-id cash-id)
                                       (or (member cr-id *revenue-accounts*)
                                           (member cr-id *receivable-accounts*)))
                                  (and (eql cr-id cash-id)
                                       (or (member db-id *payable-accounts*)
                                           (member db-id *expense-accounts*))))
                              :cash)
                             ;; cheques
                             ((intersection (list (account-id 'cheque-receivable-account)
                                                  (account-id 'cheque-payable-account))
                                            (list db-id cr-id))
                              :cheque)
                             ;; invoices
                             ((or (and (member db-id *receivable-accounts*)
                                       (member cr-id *revenue-accounts*))
                                  (and (member db-id *expense-accounts*)
                                       (member cr-id *payable-accounts*))
                                  (and (member db-id *revenue-accounts*)
                                       (member cr-id *receivable-accounts*))
                                  (and (member db-id *payable-accounts*)
                                       (member cr-id *expense-accounts*)))
                              :invoice))))
      (if (lib-p temtx)
          (values :libtx normal-section)
          (values normal-section normal-section)))))

(defun app-section-label (temtx-id)
  (multiple-value-bind (active normal) (app-section temtx-id)
    (if (eql active :libtx)
        (case normal
          (:cash "Βιβλιοθήκη (αντί για Μετρητά)")
          (:cheque "Βιβλιοθήκη (αντί για Επιταγές)")
          (:invoice "Βιβλιοθήκη (αντί για Χρεωπιστώσεις)"))
        (case active
          (:cash "Μετρητά")
          (:cheque "Επιταγές")
          (:invoice "Χρεωπιστώσεις")
          ((nil) "Χωρίς αντιστοίχιση")))))


;;; left-column

(defun left-column (form styles disabled)
  (let* ((record (record form))
         (ldfn (label-datum disabled record styles)))
    (with-html
        (:div :class "left-column"
              (:h3 "Στοιχεία Συναλλαγής")
              (display ldfn 'tx-date "Ημερομηνία" :enabled-styles "datepicker"
                                                  :default-value (today))
              (display ldfn 'company "Εταιρία"
                       :enabled-styles "ac-company"
                       :href (company/tx :company-id (getf record :company-id)
                                         :tx-id (key form))
                       :common-styles "company")
              (display ldfn 'description "Περιγραφή"
                       :common-styles "description")
              (display ldfn 'amount "Ποσό"
                       :common-styles "amount"
                       :format-fn #'fmt-amount)
              (:div :id "project-group"
                    (:label "Έργο"
                            (:div :id "project-picker" "")))
              (unless disabled
                (htm (:div :class "data-form-buttons"
                           (ok-button :body (if (eql (op form) :update)
                                                "Ανανέωση"
                                                "Δημιουργία"))
                           (cancel-button (cancel-url form)
                                          :body "Άκυρο"))))))))



;;; ----------------------------------------------------------------------
;;; Widgets
;;; ----------------------------------------------------------------------

;;; radio-account-tree

(defclass radio-account-tree (account-tree)
  ((disabled :accessor disabled :initarg :disabled))
  (:default-initargs :item-class 'radio-account-node
                     :op :catalogue
                     :disabled nil))

(defclass radio-account-node (account-node)
  ())

(defmethod selector ((node radio-account-node) selected-p)
  (make-instance 'input-radio
                 :disabled (disabled (collection node))
                 :name 'account-id
                 :value (key node)
                 :body nil
                 :checked selected-p))

(defmethod payload ((node radio-account-node) enabled-p)
  (declare (ignore enabled-p))
  (html ()
        (str (lisp->html (getf (record node) :title)))))

(defmethod controls ((node radio-account-node) controls-p)
  (declare (ignore controls-p))
  (list nil nil))
