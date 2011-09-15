(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass transaction-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id))
   (payload-parameter-names
    :allocation :class
    :initform '(date transaction description debit-account credit-account amount))
   (filter-parameter-names
    :allocation :class
    :initform '(search))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))
   (messages
    :allocation :class
    :reader messages
    :initform
    '((company
       (:company-title-unknown
        "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"))
      (amount
       (:non-positive-amount
        "Το ποσό της συναλλαγής πρέπει να είναι θετικός αριθμός"
        :parse-error
        "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες"))
      (debit-account
       (:account-title-null
        "Ο λογαριασμός χρέωσης είναι κενός"
        :account-title-unknown
        "Λάθος λογαριασμός χρέωσης: Δεν έχει καταχωρηθεί λογαριασμός με αυτό το όνομα"))
      (credit-account
       (:account-title-null
        "Ο λογαριασμός πίστωσης είναι κενός"
        :account-title-unknown
        "Λάθος λογαριασμός πίστωσης: Δεν έχει καταχωρηθεί λογαριασμός με αυτό το όνομα"))
      (date
       (:parse-error
        "Η ημερομηνία της συναλλαγής είναι άκυρη"))))))



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(define-existence-predicate tx-id-exists-p tx id)
(define-existence-predicate* tx-description-exists-p tx description id)

(defun chk-tx-id (id)
  (if (tx-id-exists-p id)
      nil
      :tx-id-unknown))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun transaction-menu (id filter &optional disabled)
  (menu (crud+details-actions-spec (apply #'transaction :id id filter)
                                   (apply #'transaction/create filter)
                                   (apply #'transaction/details :id id filter)
                                   (apply #'transaction/update  :id id filter)
                                   (apply #'transaction/delete  :id id filter))
        :id "transaction-actions"
        :css-class "hmenu actions"
        :disabled disabled))

(defun transaction-notifications ()
  (notifications
   ))



;;; ----------------------------------------------------------------------
;;; Database interface
;;; ----------------------------------------------------------------------

(defun tx-record (id)
  (with-db ()
    (query (:select 'tx.id
                    (:as 'tx-date 'date)
                    (:as 'company.title 'company)
                    'description
                    (:as 'debit-account.title 'debit-account-nonchequing)
                    (:as 'credit-account.title 'credit-account-nonchequing)
                    'tx.debit-acc-id
                    'tx.credit-acc-id
                    'amount
            :from 'tx
            :left-join 'company
            :on (:= 'tx.company-id 'company.id)
            :left-join (:as 'account 'debit-account)
            :on (:= 'debit-account.id 'debit-acc-id)
            :left-join (:as 'account 'credit-account)
            :on (:= 'credit-account.id 'credit-acc-id)
            :where (:= 'tx.id id))
           :plist)))



;;; ----------------------------------------------------------------------
;;; Transaction table
;;; ----------------------------------------------------------------------

;;; table

(defclass tx-table (scrooge-table)
  ((header-labels  :initform '("" "Ημερομηνία" "Εταιρία" "Περιγραφή" "Ποσό"))
   (paginator      :initform (make-instance 'scrooge-paginator
                                            :id "tx-paginator"
                                            :css-class "paginator"
                                            :urlfn #'transaction)))
  (:default-initargs :item-class 'tx-row))

(defmethod get-records ((table tx-table))
  (let* ((search (getf (filter table) :search))
         (base-query `(:select tx.id
                               (:as tx-date date)
                               (:as company.title company)
                               description amount
                       :from tx
                       :left-join company
                       :on (:= tx.company-id company.id)))
         (composite-query (if search
                              (append base-query
                                      `(:where (:or (:ilike description ,(ilike search))
                                                    (:ilike company.title ,(ilike search)))))
                              base-query))
         (final-query `(:order-by ,composite-query (:desc date))))
    (with-db ()
      (query (sql-compile final-query)
             :plists))))


;;; rows

(defclass tx-row (scrooge-row/plist)
  ())

(defmethod selector ((row tx-row) selected-p)
  (simple-selector row selected-p #'transaction))

(defmethod controls ((row tx-row) controls-p)
  (simple-controls row controls-p #'transaction))

(defmethod payload ((row tx-row) enabled-p)
  (let ((record (record row)))
    (mapcar (lambda (name)
              (make-instance 'textbox
                             :name name
                             :value (getf record (make-keyword name))
                             :disabled (not enabled-p)))
            '(date company description amount))))



;;; ----------------------------------------------------------------------
;;; Transaction form
;;; ----------------------------------------------------------------------

(defclass tx-form (crud-form/plist)
  ())

(defmethod display ((form tx-form) &key styles)
  (let ((disabled (eql (op form) :details))
        (record (record form)))
    (flet ((label-input-text (name label &optional extra-styles)
             (with-html
               (label name label)
               (input-text name
                           :value (getf record (make-keyword name))
                           :disabled disabled
                           :css-class (conc (getf styles (make-keyword name))
                                            " " extra-styles)))))
      (with-html
        (:div :class "data-form tx-form"
              (label-input-text 'date "Ημερομηνία" "datepicker")
              (label-input-text 'company "Εταιρία" "ac-company")
              (label-input-text 'description "Περιγραφή")
              (label-input-text 'debit-account-nonchequing "Λογαριασμός χρέωσης"
                                "ac-nonchequing-account")
              (label-input-text 'credit-account-nonchequing "Λογαριασμός πίστωσης"
                                "ac-nonchequing-account")
              (label-input-text 'amount "Ποσόν"))
        (:div :class "data-form-buttons"
              (if disabled
                  (cancel-button (cancel-url form)
                                 :body "Επιστροφή στον Κατάλογο Συναλλαγών")
                  (progn
                    (ok-button :body (if (eql (op form) :update) "Ανανέωση" "Δημιουργία"))
                    (cancel-button (cancel-url form) :body "Άκυρο"))))))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defpage dynamic-page transaction ("transaction")
    ((id     integer chk-tx-id)
     (search string)
     (start  integer))
  (with-view-page
    (let* ((filter (params->filter))
           (tx-table (make-instance 'tx-table
                                    :op :read
                                    :filter filter)))
      (with-document ()
        (:head
         (:title "Συναλλαγές")
         (financial-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'financial)
               (financial-navbar 'transaction)
               (:div :class "window grid_10"
                     (:div :class "title" "Συναλλαγές » Κατάλογος")
                     (transaction-menu (val id)
                                       filter
                                       (if (val id)
                                           '(:read)
                                           '(:read :details :update :delete)))
                     (display tx-table
                              :key (val id)
                              :start (val start)))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (transaction) (val search)))
               (footer)))))))

(defpage dynamic-page transaction/details ("transaction/details")
    ((search     string)
     (id         integer chk-tx-id t))
  (with-view-page
    (let* ((filter (params->filter))
           (transaction-form (make-instance 'transaction-form
                                            :op :read
                                            :record (get-record 'transaction (val id))
                                            :cancel-url (apply #'transaction :id (val id) filter))))
      (with-document ()
        (:head
         (:title "Συναλλαγή » Λεπτομέρειες")
         (financial-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'financial)
               (financial-navbar 'transaction)
               (:div :class "window grid_12"
                     (:div :class "title" "Συναλλαγή » Λεπτομέρειες")
                     (transaction-menu (val id)
                                       filter
                                       '(:details :create))
                     (display transaction-form :payload (tx-record (val id))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     "")))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage dynamic-page transaction/create ("transaction/create")
    ((search         string)
     (date           date)
     (description    string)
     (company        string  chk-company-title*)
     (amount         float   chk-amount)
     (debit-account  string  chk-non-chq-acc-title)
     (credit-account string  chk-non-chq-acc-title))
  (with-view-page
    (let* ((filter (params->filter))
           (transaction-form (make-instance 'transaction-form
                                            :op :create
                                            :record nil
                                            :cancel-url (apply #'transaction filter))))
      (with-document ()
        (:head
         (:title "Συναλλαγές » Δημιουργία")
         (financial-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'financial)
               (financial-navbar 'transaction)
               (:div :class "window grid_12"
                     (:div :class "title" "Συναλλαγή » Δημιουργία")
                     (transaction-menu nil
                                       filter
                                       '(:details :create :update :delete))
                     (transaction-notifications)
                     (with-form (actions/transaction/create)
                       (display transaction-form :payload (params->payload)
                                                 :styles (params->styles))))
               (footer)))))))

(defpage dynamic-page actions/transaction/create ("actions/transaction/create"
                                                  :request-type :post)
    ((search         string)
     (date           date)
     (description    string)
     (company        string  chk-company-title*)
     (amount         float   chk-amount)
     (debit-account  string  chk-non-chq-acc-title)
     (credit-account string  chk-non-chq-acc-title))
  (with-controller-page (transaction/create)
    (let* ((company-id (company-id (val company)))
           (debit-acc-id (account-id (val debit-account)))
           (credit-acc-id (account-id (val credit-account)))
           (new-tx (make-instance 'tx
                                  :tx-date (val date)
                                  :description (val description)
                                  :company-id company-id
                                  :amount (val amount)
                                  :credit-acc-id credit-acc-id
                                  :debit-acc-id debit-acc-id)))
      (insert-dao new-tx)
      (see-other (transaction :id (id new-tx) :search (val search))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage dynamic-page transaction/update ("transaction/update")
    ((search         string)
     (id             integer chk-tx-id t)
     (date           date)
     (description    string)
     (company        string  chk-company-title*)
     (amount         float   chk-amount)
     (debit-account  string  chk-non-chq-acc-title)
     (credit-account string  chk-non-chq-acc-title))
  (with-view-page
    (let* ((filter (params->filter))
           (transaction-form (make-instance 'transaction-form
                                            :op :update
                                            :record (get-record 'transaction (val id))
                                            :cancel-url (apply #'transaction filter))))
      (with-document ()
        (:head
         (:title "Συναλλαγή » Επεξεργασία")
         (financial-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'financial)
               (financial-navbar 'transaction)
               (:div :class "window grid_12"
                     (:div :class "title" "Συναλλαγή » Επεξεργασία")
                     (transaction-menu (val id)
                                       filter
                                       '(:create :update))
                     (transaction-notifications)
                     (with-form (actions/transaction/update :id (val id)
                                                            :search (val search))
                       (display transaction-form :payload (params->payload)
                                                 :styles (params->styles))))
               (footer)))))))

(defpage dynamic-page actions/transaction/update ("actions/transaction/update"
                                                  :request-type :post)
    ((search         string)
     (id             integer chk-tx-id t)
     (date           date)
     (description    string)
     (company        string  chk-company-title*)
     (amount         float   chk-amount)
     (debit-account  string  chk-non-chq-acc-title)
     (credit-account string  chk-non-chq-acc-title))
  (with-controller-page (transaction/update)
    (let ((company-id (company-id (val company)))
          (debit-acc-id (account-id (val debit-account)))
          (credit-acc-id (account-id (val credit-account))))
      (execute (:update 'tx :set
                        'tx-date (val date)
                        'description (val description)
                        'company-id company-id
                        'amount (val amount)
                        'debit-acc-id debit-acc-id
                        'credit-acc-id credit-acc-id
                        :where (:= 'id (val id))))
      (see-other (transaction :id (val id))))))



;;; -----------------------------------------------------------------------
;;; DELETE
;;; -----------------------------------------------------------------------

(defpage dynamic-page transaction/delete ("transaction/delete")
    ((id     integer chk-tx-id t)
     (search string))
  (with-view-page
    (let* ((filter (params->filter))
           (tx-table (make-instance 'tx-table
                                    :op :delete
                                    :filter filter)))
      (with-document ()
        (:head
         (:title "Συναλλαγή » Διαγραφή")
         (financial-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'financial)
               (financial-navbar 'transaction)
               (:div :class "window grid_10"
                     (:div :class "title" "Συναλλαγή » Διαγραφή")
                     (transaction-menu (val id)
                                       filter
                                       '(:create :delete))
                     (with-form (actions/transaction/delete :id (val id)
                                                            :search (val search))
                       (display tx-table :key (val id))))
               (:div :id "sidebar" :class "sidebar `grid_2"
                     (searchbox (transaction) (val search)))
               (footer)))))))

(defpage dynamic-page actions/transaction/delete ("actions/transaction/delete"
                                                  :request-type :post)
    ((id     integer chk-tx-id t)
     (search string))
  (with-controller-page (transaction/delete)
    (delete-dao (get-dao 'tx (val id)))
    (see-other (transaction :search (val search)))))
