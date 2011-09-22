(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass tx-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id))
   (payload-parameter-names
    :allocation :class
    :initform '(date description company amount non-chq-debit-acc non-chq-credit-acc))
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
        "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"
        :company-title-null
        "Η επωνυμία της εταιρίας είναι κενή"))
      (amount
       (:empty-amount
        "Το ποσό της συναλλαγής είναι κενό"
        :non-positive-amount
        "Το ποσό της συναλλαγής δεν είναι θετικός αριθμός"
        :parse-error
        "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες"))
      (non-chq-debit-acc
       (:account-title-null
        "Ο λογαριασμός χρέωσης είναι κενός"
        :account-title-unknown
        "Λάθος λογαριασμός χρέωσης: Δεν έχει καταχωρηθεί λογαριασμός με αυτό το όνομα"))
      (non-chq-credit-acc
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

(defun tx-menu (id filter &optional disabled)
  (anchor-menu (crud+details-actions-spec (apply #'tx :id id filter)
                                          (apply #'tx/create filter)
                                          (apply #'tx/details :id id filter)
                                          (apply #'tx/update  :id id filter)
                                          (apply #'tx/delete  :id id filter))
               :id "tx-actions"
               :css-class "hmenu actions"
               :disabled disabled))



;;; ----------------------------------------------------------------------
;;; TX form
;;; ----------------------------------------------------------------------

(defclass tx-form (crud-form/plist)
  ())

(defmethod display ((form tx-form) &key styles)
  (let* ((disabled (eql (op form) :details))
         (record (record form))
         (lit (label-input-text disabled record styles)))
    (with-html
      (:div :class "data-form tx-form"
            (display lit 'date "Ημερομηνία" "datepicker")
            (display lit 'company "Εταιρία" "ac-company")
            (display lit 'description "Περιγραφή")
            (display lit 'non-chq-debit-acc "Λογαριασμός χρέωσης" "ac-non-chq-account")
            (display lit 'non-chq-credit-acc "Λογαριασμός πίστωσης" "ac-non-chq-account")
            (display lit 'amount "Ποσό"))
      (:div :class "data-form-buttons"
            (if disabled
                (cancel-button (cancel-url form)
                               :body "Επιστροφή στον Κατάλογο Συναλλαγών")
                (progn
                  (ok-button :body (if (eql (op form) :update) "Ανανέωση" "Δημιουργία"))
                  (cancel-button (cancel-url form) :body "Άκυρο")))))))

(defmethod get-record ((type (eql 'tx)) id)
  (declare (ignore type))
  (with-db ()
    (query (:select 'tx.id
                    (:as 'tx-date 'date)
                    (:as 'company.title 'company)
                    'description
                    (:as 'non-chq-debit-acc.title 'non-chq-debit-acc)
                    (:as 'non-chq-credit-acc.title 'non-chq-credit-acc)
                    'tx.debit-acc-id
                    'tx.credit-acc-id
                    'amount
            :from 'tx
            :left-join 'company
            :on (:= 'tx.company-id 'company.id)
            :left-join (:as 'account 'non-chq-debit-acc)
            :on (:= 'non-chq-debit-acc.id 'debit-acc-id)
            :left-join (:as 'account 'non-chq-credit-acc)
            :on (:= 'non-chq-credit-acc.id 'credit-acc-id)
            :where (:= 'tx.id id))
           :plist)))



;;; ----------------------------------------------------------------------
;;; TX table
;;; ----------------------------------------------------------------------

;;; table

(defclass tx-table (scrooge-table)
  ((header-labels  :initform '("" "Ημερομηνία" "Εταιρία" "Περιγραφή" "Ποσό"))
   (paginator      :initform (make-instance 'scrooge-paginator
                                            :id "tx-paginator"
                                            :css-class "paginator")))
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
  (simple-selector row selected-p #'tx))

(defmethod controls ((row tx-row) controls-p)
  (simple-controls row controls-p #'tx))

(defmethod payload ((row tx-row) enabled-p)
  (let ((record (record row)))
    (mapcar (lambda (name)
              (make-instance 'textbox
                             :name name
                             :value (getf record (make-keyword name))
                             :disabled (not enabled-p)))
            '(date company description amount))))

;;; paginator

(defclass tx-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg tx-paginator) start)
  (apply #'tx :start start (filter (table pg))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defpage tx-page tx ("tx")
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
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'tx)
               (:div :class "window grid_10"
                     (:div :class "title" "Συναλλαγές » Κατάλογος")
                     (tx-menu (val id)
                              filter
                              (if (val id)
                                  '(:read)
                                  '(:read :details :update :delete)))
                     (display tx-table
                              :key (val id)
                              :start (val start)))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (tx) (val search)))
               (footer)))))))

(defpage tx-page tx/details ("tx/details")
    ((search     string)
     (id         integer chk-tx-id t))
  (with-view-page
    (let* ((filter (params->filter))
           (tx-form (make-instance 'tx-form
                                   :op :read
                                   :record (get-record 'tx (val id))
                                   :cancel-url (apply #'tx :id (val id) filter))))
      (with-document ()
        (:head
         (:title "Συναλλαγή » Λεπτομέρειες")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'tx)
               (:div :class "window grid_12"
                     (:div :class "title" "Συναλλαγή » Λεπτομέρειες")
                     (tx-menu (val id)
                              filter
                              '(:details :create))
                     (display tx-form :payload (get-record 'tx (val id))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     "")))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage tx-page tx/create ("tx/create")
    ((search             string)
     (date               date)
     (description        string)
     (company            string chk-company-title)
     (amount             float  chk-amount)
     (non-chq-debit-acc  string chk-non-chq-acc-title)
     (non-chq-credit-acc string chk-non-chq-acc-title))
  (with-view-page
    (let* ((filter (params->filter))
           (tx-form (make-instance 'tx-form
                                   :op :create
                                   :record nil
                                   :cancel-url (apply #'tx filter))))
      (with-document ()
        (:head
         (:title "Συναλλαγές » Δημιουργία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'tx)
               (:div :class "window grid_12"
                     (:div :class "title" "Συναλλαγή » Δημιουργία")
                     (tx-menu nil
                              filter
                              '(:details :create :update :delete))
                     (notifications)
                     (with-form (actions/tx/create)
                       (display tx-form :payload (params->payload)
                                        :styles (params->styles))))
               (footer)))))))

(defpage tx-page actions/tx/create ("actions/tx/create"
                                    :request-type :post)
    ((search         string)
     (date           date)
     (description    string)
     (company        string  chk-company-title)
     (amount         float   chk-amount)
     (non-chq-debit-acc  string  chk-non-chq-acc-title)
     (non-chq-credit-acc string  chk-non-chq-acc-title))
  (with-controller-page (tx/create)
    (let* ((company-id (company-id (val company)))
           (debit-acc-id (account-id (val non-chq-debit-acc)))
           (credit-acc-id (account-id (val non-chq-credit-acc)))
           (new-tx (make-instance 'tx
                                  :tx-date (val date)
                                  :description (val description)
                                  :company-id company-id
                                  :amount (val amount)
                                  :credit-acc-id credit-acc-id
                                  :debit-acc-id debit-acc-id)))
      (insert-dao new-tx)
      (see-other (tx :id (tx-id new-tx) :search (val search))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage tx-page tx/update ("tx/update")
    ((search             string)
     (id                 integer chk-tx-id             t)
     (date               date)
     (description        string)
     (company            string  chk-company-title)
     (amount             float   chk-amount)
     (non-chq-debit-acc  string  chk-non-chq-acc-title)
     (non-chq-credit-acc string  chk-non-chq-acc-title))
  (with-view-page
    (let* ((filter (params->filter))
           (tx-form (make-instance 'tx-form
                                   :op :update
                                   :record (get-record 'tx (val id))
                                   :cancel-url (apply #'tx filter))))
      (with-document ()
        (:head
         (:title "Συναλλαγή » Επεξεργασία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'tx)
               (:div :class "window grid_12"
                     (:div :class "title" "Συναλλαγή » Επεξεργασία")
                     (tx-menu (val id)
                              filter
                              '(:create :update))
                     (notifications)
                     (with-form (actions/tx/update :id (val id)
                                                   :search (val search))
                       (display tx-form :payload (params->payload)
                                        :styles (params->styles))))
               (footer)))))))

(defpage tx-page actions/tx/update ("actions/tx/update"
                                    :request-type :post)
    ((search         string)
     (id             integer chk-tx-id t)
     (date           date)
     (description    string)
     (company        string  chk-company-title)
     (amount         float   chk-amount)
     (non-chq-debit-acc  string  chk-non-chq-acc-title)
     (non-chq-credit-acc string  chk-non-chq-acc-title))
  (with-controller-page (tx/update)
    (let ((company-id (company-id (val company)))
          (debit-acc-id (account-id (val non-chq-debit-acc)))
          (credit-acc-id (account-id (val non-chq-credit-acc))))
      (execute (:update 'tx :set
                        'tx-date (val date)
                        'description (val description)
                        'company-id company-id
                        'amount (val amount)
                        'debit-acc-id debit-acc-id
                        'credit-acc-id credit-acc-id
                        :where (:= 'id (val id))))
      (see-other (tx :id (val id))))))



;;; -----------------------------------------------------------------------
;;; DELETE
;;; -----------------------------------------------------------------------

(defpage tx-page tx/delete ("tx/delete")
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
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'tx)
               (:div :class "window grid_10"
                     (:div :class "title" "Συναλλαγή » Διαγραφή")
                     (tx-menu (val id)
                              filter
                              '(:create :delete))
                     (with-form (actions/tx/delete :id (val id)
                                                   :search (val search))
                       (display tx-table :key (val id))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (tx) (val search)))
               (footer)))))))

(defpage tx-page actions/tx/delete ("actions/tx/delete"
                                    :request-type :post)
    ((id     integer chk-tx-id t)
     (search string))
  (with-controller-page (tx/delete)
    (delete-dao (get-dao 'tx (val id)))
    (see-other (tx :search (val search)))))
