(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(define-existence-predicate tx-id-exists-p tx id)
(define-uniqueness-predicate tx-description-unique-p tx description id)

(defun chk-tx-id (id)
  (if (tx-id-exists-p id)
      nil
      :tx-id-unknown))



;;; ----------------------------------------------------------------------
;;; Actions
;;; ----------------------------------------------------------------------

(define-dynamic-page actions/transaction/create ("actions/transaction/create" :request-type :post)
    ((filter         string)
     (date           string  chk-date)
     (description    string)
     (company        string  chk-company-title)
     (amount         integer chk-amount)
     (debit-account  string  chk-acc-title)
     (credit-account string  chk-acc-title))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (let* ((company-id (company-id company))
             (debit-acc-id (account-id debit-account))
             (credit-acc-id (account-id credit-account))
             (new-tx (make-instance 'tx
                                    :date (val date)
                                    :description (val description)
                                    :company-id company-id
                                    :amount (val amount)
                                    :credit-acc-id credit-acc-id
                                    :debit-acc-id debit-acc-id)))
        (with-db ()
          (insert-dao new-tx)
          (see-other (transaction :id (id new-tx) :filter (val filter)))))
      (see-other (transaction/create :date (raw date)
                                     :description (raw description)
                                     :company (raw company)
                                     :amount (raw amount)
                                     :debit-account (raw debit-account)
                                     :credit-account (raw credit-account)))))

(define-dynamic-page actions/transaction/update ("actions/transaction/update" :request-type :post)
    ((id             integer chk-tx-id t)
     (date           string  chk-date)
     (description    string)
     (company        string  chk-company-title)
     (amount         integer chk-amount)
     (debit-account  string  chk-acc-title)
     (credit-account string  chk-acc-title))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (let ((company-id (company-id company))
            (debit-acc-id (account-id debit-account))
            (credit-acc-id (account-id credit-account)))
        (with-db ()
          (execute (:update 'tx :set
                            'date (val date)
                            'description (val description)
                            'company-id company-id
                            'amount (val amount)
                            'debit-acc-id debit-acc-id
                            'credit-acc-id credit-acc-id
                            :where (:= 'id (val id))))
          (see-other (transaction :id id))))
      (see-other (transaction/update :id (raw id)
                                     :date (raw date)
                                     :description (raw description)
                                     :company (raw company)
                                     :amount (raw amount)
                                     :debit-account (raw debit-account)
                                     :credit-account (raw credit-account)))))

(define-dynamic-page actions/transaction/delete ("actions/transaction/delete" :request-type :post)
    ((id     integer chk-tx-id t)
     (filter string))
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'tx (val id)))
        (see-other (transaction :filter (val filter))))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; Transaction menu
;;; ------------------------------------------------------------

(defun transaction-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "transaction-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (standard-actions-spec (transaction :id id
                                                                    :filter filter)
                                                       (transaction/create :filter filter)
                                                       (transaction/update :id id
                                                                           :filter filter)
                                                       (transaction/delete :id id
                                                                           :filter filter)))
           :disabled-items disabled-items))



;;; ------------------------------------------------------------
;;; Database interface
;;; ------------------------------------------------------------

(defun get-tx-plists (filter)
  (let* ((base-query `(:select tx.id (:as tx-date date)
                               (:as company.title company)
                               description amount
                               (:as debit-account.title debit-account)
                               (:as credit-account.title credit-account)
                               :from tx
                               :left-join company
                               :on (:= tx.company-id company.id)
                               :left-join (:as account debit-account)
                               :on (:= debit-account.id debit-acc-id)
                               :left-join (:as account credit-account)
                               :on (:= credit-account.id credit-acc-id)))
         (composite-query (if filter
                              (append base-query
                                      `(where (:or (:ilike description ,(ilike filter))
                                                   (:ilike company.title ,(ilike filter))
                                                   (:ilike amount ,(ilike filter)))))
                              base-query))
         (final-query `(:order-by ,composite-query date)))
    (with-db ()
      (query (sql-compile final-query)
             :plists))))



;;; ------------------------------------------------------------
;;; Transaction table
;;; ------------------------------------------------------------

;;; table

(defclass tx-table (crud-table)
  ((header-labels :initform '("" "Ημερομηνία" "Εταιρία" "Περιγραφή" "Ποσό"
                              "Λογαριασμός χρέωσης" "Λογαριασμός πίστωσης"))
   (paginator     :initform (make-instance 'paginator
                                           :id "tx-paginator"
                                           :style "paginator grid_9 alpha"
                                           :delta 10
                                           :urlfn (lambda (filter start)
                                                    (transaction :filter filter
                                                                 :start start))))))

(defmethod read-items ((table tx-table))
  (iter (for rec in (get-tx-plists (filter table)))
        (for i from 0)
        (collect (make-instance 'tx-row
                                :key (getf rec :id)
                                :record rec
                                :collection table
                                :index i))))

(defmethod insert-item ((table tx-table) &key record index)
  (let* ((rows (rows table))
         (new-row (make-instance 'tx-row
                                 :key (getf record :id)
                                 :record record
                                 :collection table
                                 :index index)))
    (setf (rows table)
          (ninsert-list index new-row rows))))


;;; rows

(defclass tx-row (crud-row)
  ())

(defmethod cells ((row tx-row) &key start)
  (let* ((id (key row))
         (record (record row))
         (pg (paginator (collection row)))
         (filter (filter (collection row))))
    (list :selector
          (make-instance 'selector-cell
                         :states (list :on (transaction :filter filter
                                                        :start (page-start pg (index row) start))
                                       :off (bank :filter filter
                                                  :id id)))
          :payload
          (mapcar (lambda (name)
                    (make-instance 'textbox-cell
                                   :name name
                                   :value (getf record (make-keyword name))))
                  '(date company description debit-account credit-account amount))
          :controls
          (list
           (make-instance 'ok-cell)
           (make-instance 'cancel-cell
                          :href (transaction :id id :filter filter))))))



;;; ------------------------------------------------------------
;;; Other areas
;;; ------------------------------------------------------------

(defun transaction-filters (filter)
  (with-html
    (:div :id "filters"
          (:p :class "title" "Φίλτρα")
          (with-form (transaction)
            (htm
             (:p :class "search"
                 (textbox 'filter :value filter)
                 (submit (html ()
                           (img "magnifier.png")))))))))

(defun transaction-notifications (&rest params)
  ;; date errors missing, system is supposed to respond with the default (error-type param)
  (notifications
   '(company    ((:company-title-invalid "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"))
     price      ((:invalid-amount  "Η τιμή πρέπει να είναι θετικός αριθμός ή μηδέν"))
     debit-acc  ((:account))
     credit-acc ())
   params))



;;; ------------------------------------------------------------
;;; Pages
;;; ------------------------------------------------------------

(define-dynamic-page transaction ("transaction/")
    ((id     integer chk-tx-id)
     (filter string)
     (start  integer))
  (no-cache)
  (if (validp id)
      (let ((tx-table (make-instance 'tx-table
                                     :op 'catalogue
                                     :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Συναλλαγές")
           (main-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'main)
                 (main-menu 'tx)
                 (:div :id "controls" :class "controls grid_3"
                       (transaction-filters (val filter)))
                 (:div :id "transaction-window" :class "window grid_9"
                       (:div :class "title" "Κατάλογος συναλλαγών")
                       (transaction-menu (val id)
                                         (val filter)
                                         (if (val id)
                                             '(catalogue)
                                             '(catalogue update delete)))
                       (display tx-table
                                :selected-id (val* id)
                                :start (val* start)))
                 (footer)))))
      (see-other (notfound))))

(define-dynamic-page transaction/create ("transaction/create")
    ((filter         string)
     (date           string  chk-date)
     (description    string)
     (company        string  chk-company-title)
     (amount         integer chk-amount)
     (debit-account  string  chk-acc-title)
     (credit-account string  chk-acc-title))
  (no-cache)
  (let ((tx-table (make-instance 'tx-table
                                 :op 'create
                                 :filter (val* filter))))
    (with-document ()
      (:head
       (:title "Δημιουργία τράπεζας")
       (config-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'config)
             (config-menu 'tx)
             (:div :id "controls" :class "controls grid_3"
                   (transaction-filters (val filter))
                   (transaction-notifications date company amount debit-account credit-account))
             (:div :id "tx-window" :class "window grid_9"
                   (:div :class "title" "Δημιουργία συναλλαγής")
                   (transaction-menu nil
                                     (val filter)
                                     '(create update delete))
                   (with-form (actions/transaction/create)
                     (display tx-table
                              :selected-id nil
                              :selected-data (list :date (val* date)
                                                   :company (val* company)
                                                   :description (val* description)
                                                   :debit-account (val* debit-account)
                                                   :credit-account (val* credit-account)
                                                   :amount (val* amount)))))
             (footer))))))

(define-dynamic-page transaction/update ("transaction/update")
    ((filter         string)
     (id             integer chk-tx-id t)
     (date           string  chk-date)
     (description    string)
     (company        string  chk-company-title)
     (amount         integer chk-amount)
     (debit-account  string  chk-acc-title)
     (credit-account string  chk-acc-title))
  (no-cache)
  (if (validp id)
      (let ((tx-table (make-instance 'tx-table
                                     :op 'update
                                     :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Επεξεργασία συναλλαγής")
           (main-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (main-menu 'transactions)
                 (:div :id "controls" :class "controls grid_3"
                       (transaction-filters (val filter))
                       (transaction-notifications date company amount debit-account credit-account))
                 (:div :id "tx-window" :class "window grid_9"
                       (:div :class "title" "Επεξεργασία συναλλαγής")
                       (transaction-menu (val id)
                                         (val filter)
                                         '(create update))
                       (with-form (actions/transaction/update :id (val* id)
                                                              :filter (val* filter))
                         (display tx-table
                                  :selected-id (val id)
                                  :selected-data (list :date (val* date)
                                                       :company (val* company)
                                                       :description (val* description)
                                                       :debit-account (val* debit-account)
                                                       :credit-account (val* credit-account)
                                                       :amount (val* amount)))))
                 (footer)))))
      (see-other (notfound))))

(define-dynamic-page transaction/delete ("transaction/delete")
    ((id     integer chk-tx-id t)
     (filter string))
  (no-cache)
  (if (validp id)
      (let ((tx-table (make-instance 'tx-table
                                     :op 'delete
                                     :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Διαγραφή τράπεζας")
           (main-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-menu 'tx)
                 (:div :id "controls" :class "controls grid_3"
                       (transaction-filters (val filter)))
                 (:div :id "tx-window" :class "window grid_9"
                       (:div :class "title" "Διαγραφή συναλλαγής")
                       (transaction-menu (val id)
                                         (val filter)
                                         '(create delete))
                       (with-form (actions/tx/delete :id (val id)
                                                     :filter (val* filter))
                         (display tx-table
                                  :selected-id (val id))))
                 (footer)))))
      (see-other (notfound))))
