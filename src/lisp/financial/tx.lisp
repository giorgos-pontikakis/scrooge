(in-package :scrooge)



;;; --------------------------------------------------------------------------------
;;; Validation
;;; --------------------------------------------------------------------------------

(define-existence-predicate tx-id-exists-p tx id)
(define-uniqueness-predicate tx-description-unique-p tx description id)

(defun chk-tx-id (id)
  (if (tx-id-exists-p id)
      nil
      :tx-id-unknown))



;;; --------------------------------------------------------------------------------
;;; Actions
;;; --------------------------------------------------------------------------------

(defpage dynamic-page actions/financial/transaction/create ("actions/financial/transaction/create"
                                                           :request-type :post)
    ((search         string)
     (date           date)
     (description    string)
     (company        string  chk-company-title*)
     (amount         float   chk-amount)
     (debit-account  string  chk-acc-title-nc)
     (credit-account string  chk-acc-title-nc))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let* ((company-id (company-id (val company))) ;; using val (accept null values)
               (debit-acc-id (account-id (val debit-account)))
               (credit-acc-id (account-id (val credit-account)))
               (new-tx (make-instance 'tx
                                      :tx-date (val date)
                                      :description (val description)
                                      :company-id company-id
                                      :amount (val amount)
                                      :credit-acc-id credit-acc-id
                                      :debit-acc-id debit-acc-id)))
          (with-db ()
            (insert-dao new-tx)
            (see-other (transaction :id (id new-tx) :search (val search)))))
        (see-other (transaction/create :date (raw date)
                                       :description (raw description)
                                       :company (raw company)
                                       :amount (raw amount)
                                       :debit-account (raw debit-account)
                                       :credit-account (raw credit-account))))))

(defpage dynamic-page actions/financial/transaction/update ("actions/financial/transaction/update"
                                                           :request-type :post)
    ((search         string)
     (id             integer chk-tx-id t)
     (date           date)
     (description    string)
     (company        string  chk-company-title*)
     (amount         float   chk-amount)
     (debit-account  string  chk-acc-title-nc)
     (credit-account string  chk-acc-title-nc))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let ((company-id (company-id (val company))) ;; using val (accept null values)
              (debit-acc-id (account-id (val debit-account)))
              (credit-acc-id (account-id (val credit-account))))
          (with-db ()
            (execute (:update 'tx :set
                              'tx-date (val date)
                              'description (val description)
                              'company-id company-id
                              'amount (val amount)
                              'debit-acc-id debit-acc-id
                              'credit-acc-id credit-acc-id
                              :where (:= 'id (val id))))
            (see-other (transaction :id (val id)))))
        (see-other (transaction/update :search (raw search)
                                       :id (raw id)
                                       :date (raw date)
                                       :description (raw description)
                                       :company (raw company)
                                       :amount (raw amount)
                                       :debit-account (raw debit-account)
                                       :credit-account (raw credit-account))))))

(defpage dynamic-page actions/financial/transaction/delete ("actions/financial/transaction/delete"
                                                           :request-type :post)
    ((id     integer chk-tx-id t)
     (search string))
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'tx (val id)))
        (see-other (transaction :search (val search))))
      (see-other (notfound))))



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
   '((company (:company-title-unknown "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"))
     (amount (:non-positive-amount  "Το ποσό της συναλλαγής πρέπει να είναι θετικός αριθμός"
              :parse-error  "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες"))
     (debit-account (:account-title-null "Ο λογαριασμός χρέωσης είναι κενός"
                     :account-title-unknown "Λάθος λογαριασμός χρέωσης: Δεν έχει καταχωρηθεί λογαριασμός με αυτό το όνομα"))
     (credit-account (:account-title-null "Ο λογαριασμός πίστωσης είναι κενός"
                      :account-title-unknown "Λάθος λογαριασμός πίστωσης: Δεν έχει καταχωρηθεί λογαριασμός με αυτό το όνομα"))
     (date (:parse-error "Η ημερομηνία της συναλλαγής είναι άκυρη")))))



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

(defmethod read-records ((table tx-table))
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

(define-selector tx-row transaction)

(defmethod payload ((row tx-row) enabled-p)
  (let ((record (record row)))
    (mapcar (lambda (name)
              (make-instance 'textbox
                             :name name
                             :value (getf record (make-keyword name))
                             :disabled (not enabled-p)))
            '(date company description amount))))

(define-controls tx-row transaction)


;;; ----------------------------------------------------------------------
;;; Pages
;;; ----------------------------------------------------------------------

(defpage dynamic-page transaction ("financial/transaction")
    ((id     integer chk-tx-id)
     (search string)
     (start  integer))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (params->payload search))
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
                                  :selected-id (val id)
                                  :start (val start)))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (transaction) (val search)))
                   (footer)))))
        (see-other (notfound)))))

(defpage dynamic-page transaction/create ("financial/transaction/create")
    ((search         string)
     (date           date)
     (description    string)
     (company        string  chk-company-title*)
     (amount         float   chk-amount)
     (debit-account  string  chk-acc-title-nc)
     (credit-account string  chk-acc-title-nc))
  (with-auth ("configuration")
    (no-cache)
    (let ((filter (params->payload search)))
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
                     (with-form (actions/financial/transaction/create)
                       (transaction-data-form :create
                                              :filter filter
                                              :data (params->payload date
                                                                       company
                                                                       description
                                                                       debit-account
                                                                       credit-account
                                                                       amount)
                                              :styles (params->styles date
                                                                          company
                                                                          description
                                                                          debit-account
                                                                          credit-account
                                                                          amount))))
               (footer)))))))

(defpage dynamic-page transaction/update ("financial/transaction/update")
    ((search         string)
     (id             integer chk-tx-id t)
     (date           date)
     (description    string)
     (company        string  chk-company-title*)
     (amount         float   chk-amount)
     (debit-account  string  chk-acc-title-nc)
     (credit-account string  chk-acc-title-nc))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((filter (params->payload search)))
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
                         (with-form (actions/financial/transaction/update :id (val id)
                                                                          :search (val search))
                           (transaction-data-form :update
                                                  :id (val id)
                                                  :filter filter
                                                  :data (plist-union
                                                         (params->payload date
                                                                            company
                                                                            description
                                                                            debit-account
                                                                            credit-account
                                                                            amount)
                                                         (tx-record (val id)))
                                                  :styles (params->styles date
                                                                              company
                                                                              description
                                                                              debit-account
                                                                              credit-account
                                                                              amount))))
                   (footer)))))
        (see-other (notfound)))))

(defpage dynamic-page transaction/details ("financial/transaction/details")
    ((search     string)
     (id         integer chk-tx-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((filter (params->payload search)))
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
                         (transaction-data-form :details
                                                :filter filter
                                                :id (val id)
                                                :data (tx-record (val id))))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         "")
                   (error-page)))))
        (see-other (notfound)))))

(defpage dynamic-page transaction/delete ("financial/transaction/delete")
    ((id     integer chk-tx-id t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (params->payload search))
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
                         (with-form (actions/financial/transaction/delete :id (val id)
                                                                          :search (val search))
                           (display tx-table :selected-id (val id))))
                   (:div :id "sidebar" :class "sidebar `grid_2"
                         (searchbox (transaction) (val search)))
                   (footer)))))
        (see-other (notfound)))))

(defun transaction-data-form (op &key filter id data styles)
  (let ((disabled (eql op :details)))
    (flet ((label-input-text (name label &optional extra-styles)
             (with-html
               (label name label)
               (input-text name
                           :value (getf data (make-keyword name))
                           :disabled disabled
                           :css-class (conc (getf styles (make-keyword name))
                                            " " extra-styles)))))
      (with-html
        (:div :id "transaction-data-form" :class "data-form"
              (:div :id "transaction-description" :class "grid_12 alpha"
                    (label-input-text 'date "Ημερομηνία" "datepicker")
                    (label-input-text 'company "Εταιρία" "ac-company")
                    (label-input-text 'description "Περιγραφή")
                    (label-input-text 'debit-account-nonchequing "Λογαριασμός χρέωσης"
                                      "ac-nonchequing-account")
                    (label-input-text 'credit-account-nonchequing "Λογαριασμός πίστωσης"
                                      "ac-nonchequing-account")
                    (label-input-text 'amount "Ποσόν")))
        (:div :class "grid_12 data-form-buttons"
              (if disabled
                  (cancel-button (apply #'transaction :id id filter)
                                 :body "Επιστροφή στον Κατάλογο Συναλλαγών")
                  (progn
                    (ok-button :body (if (eql op :update) "Ανανέωση" "Δημιουργία"))
                    (cancel-button (apply #'transaction :id id filter) :body "Άκυρο"))))))))