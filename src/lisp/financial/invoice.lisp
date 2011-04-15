(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Checks
;;; ----------------------------------------------------------------------

(defun check-invoice-accounts ()
  (unless (and *invoice-receivable-account*
               *invoice-payable-account*
               *revenues-root-account*
               *expenses-root-account*)
    (see-other (invoice-accounts-error-page))))

(define-dynamic-page invoice-accounts-error-page ("financial/invoice/error")
    ()
  (with-document ()
    (:head
     (:title "Invoice accounts error")
     (error-headers))
    (:body
     (:div :id "header"
           (logo))
     (:div :id "body"
           (:div :id "content" :class "summary"
                 (:p "Δεν έχετε ορίσει στις ρυθμίσεις είτε ένα από τους λογαριασμούς ποσών προς είσπραξη/πληρωμή, ή έναν από τους λογαριασμούς ρίζας εσόδων/εξόδων."))))))



;;; ----------------------------------------------------------------------
;;; Actions
;;; ----------------------------------------------------------------------

(define-regex-page actions/financial/invoice/create
    (("actions/financial/invoice/" (invoice-kind "(receivable|payable)") "/create")
     :request-type :post)
    ((search      string)
     (date        date)
     (company     string  chk-company-title*)
     (description string)
     (amount      float   chk-amount)
     (account-id  integer chk-acc-id t))
  (with-auth ("configuration")
    (no-cache)
    (check-invoice-accounts)
    (if (every #'validp (parameters *page*))
        (let* ((company-id (company-id (val* company))) ;; using val* (accept null values)
               (debit-acc-id (if (string-equal invoice-kind "receivable")
                                 *invoice-receivable-account*
                                 (val account-id)))
               (credit-acc-id (if (string-equal invoice-kind "receivable")
                                  (val account-id)
                                  *invoice-payable-account*))
               (new-tx (make-instance 'tx
                                      :tx-date (val date)
                                      :description (val description)
                                      :company-id company-id
                                      :amount (val amount)
                                      :credit-acc-id credit-acc-id
                                      :debit-acc-id debit-acc-id)))
          (with-db ()
            (insert-dao new-tx)
            (see-other (invoice invoice-kind :id (id new-tx) :search (val search)))))
        (see-other (invoice/create invoice-kind
                                   :date (raw date)
                                   :description (raw description)
                                   :company (raw company)
                                   :amount (raw amount)
                                   :account-id (raw account-id)
                                   :search (raw search))))))

(define-regex-page actions/financial/invoice/update
    (("actions/financial/invoice/" (invoice-kind "receivable|payable") "/update")
     :request-type :post)
    ((search      string)
     (id          integer chk-tx-id t)
     (date        date)
     (description string)
     (company     string  chk-company-title*)
     (amount      float   chk-amount)
     (account-id  integer chk-acc-id))
  (with-auth ("configuration")
    (no-cache)
    (check-invoice-accounts)
    (if (every #'validp (parameters *page*))
        (let ((company-id (company-id (val* company))) ;; using val* (accept null values)
              (debit-acc-id (if (string-equal invoice-kind "receivable")
                                *invoice-receivable-account*
                                (val account-id)))
              (credit-acc-id (if (string-equal invoice-kind "receivable")
                                 (val account-id)
                                 *invoice-payable-account*)))
          (with-db ()
            (execute (:update 'tx :set
                              'tx-date (val date)
                              'description (val description)
                              'company-id company-id
                              'amount (val amount)
                              'debit-acc-id debit-acc-id
                              'credit-acc-id credit-acc-id
                              :where (:= 'id (val id))))
            (see-other (invoice invoice-kind :id (val id)))))
        (see-other (invoice/update invoice-kind
                                   :search (raw search)
                                   :id (raw id)
                                   :date (raw date)
                                   :description (raw description)
                                   :company (raw company)
                                   :amount (raw amount)
                                   :account-id (raw account-id))))))

(define-regex-page actions/financial/invoice/delete
    (("actions/financial/invoice/" (invoice-kind "receivable|payable") "/delete") :request-type :post)
    ((id     integer chk-tx-id t)
     (search string))
  (with-auth ("configuration")
    (check-invoice-accounts)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'tx (val id)))
          (see-other (invoice invoice-kind :search (val search))))
        (see-other (notfound)))))



;;; ----------------------------------------------------------------------
;;; Invoice transactions table
;;; ----------------------------------------------------------------------

(defclass invoice-tx-table (tx-table)
  ((item-key-field :initform :id)
   (subpage :accessor subpage :initarg :subpage)
   (paginator :initform (make-instance 'paginator
                                       :id "invoice-tx-paginator"
                                       :style "paginator")))
  (:default-initargs :item-class 'invoice-tx-row))

(defmethod initialize-instance :after ((table invoice-tx-table) &key)
  (let ((invoice-kind (subpage table)))
    (setf (urlfn (paginator table))
          (lambda (&rest args)
            (apply #'invoice invoice-kind args)))))

(defmethod read-records ((table invoice-tx-table))
  (flet ((invoice-kind-account (invoice-kind)
           (cond ((string-equal invoice-kind "receivable") `(debit-acc-id ,*invoice-receivable-account*))
                 ((string-equal invoice-kind "payable") `(credit-acc-id ,*invoice-payable-account*))
                 (t (error "internal error in invoice-kind-account")))))
    (let* ((search (getf (filter table) :search))
           (invoice-kind (subpage table))
           (base-query `(:select tx.id
                                 (:as tx-date date)
                                 (:as company.title company)
                                 description amount
                                 :from tx
                                 :left-join company
                                 :on (:= tx.company-id company.id)))
           (composite-query
            (if search
                (append base-query
                        `(:where (:and (:= ,@(invoice-kind-account invoice-kind))
                                       (:or (:ilike description ,(ilike search))
                                            (:ilike company.title ,(ilike search))))))
                (append base-query
                        `(:where (:or (:= ,@(invoice-kind-account invoice-kind)))))))
           (final-query `(:order-by ,composite-query (:desc date))))
      (with-db ()
        (query (sql-compile final-query)
               :plists)))))


;;; rows

(defclass invoice-tx-row (tx-row)
  ())

(defmethod cells ((row invoice-tx-row) &key start)
  (let* ((id (key row))
         (record (record row))
         (pg (paginator (collection row)))
         (filter (filter (collection row)))
         (invoice-kind (subpage (collection row))))
    (list :selector
          (make-instance 'selector-cell
                         :states (list :on (apply #'invoice invoice-kind
                                                  :start (page-start pg (index row) start)
                                                  filter)
                                       :off (apply #'invoice invoice-kind :id id filter)))
          :payload
          (mapcar (lambda (name)
                    (make-instance 'textbox-cell
                                   :name name
                                   :value (getf record (make-keyword name))))
                  '(date company description amount))
          :controls
          (list
           (make-instance 'ok-cell)
           (make-instance 'cancel-cell
                          :href (apply #'invoice invoice-kind :id id filter))))))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun invoice-kind-label (invoice-kind)
  (cond ((string-equal invoice-kind "receivable") "Προς είσπραξη")
        ((string-equal invoice-kind "payable") "Προς πληρωμή")
        (t (error "Internal error in invoice-kind-label"))))

(defun invoice-filters (invoice-kind search)
  (let ((spec `((receivable ,(invoice "receivable" :search search) "Προς είσπραξη")
                (payable ,(invoice "payable" :search search) "Προς πληρωμή"))))
    (with-html
      (:div :id "filters" :class "filters"
            (:p :class "title" "Κατάσταση")
            (display
             (make-instance 'vertical-navbar
                            :id "invoice-filters"
                            :style "vnavbar"
                            :spec spec)
             :active-page-name (intern (string-upcase invoice-kind)))))))

(defun invoice-menu (invoice-kind id filter disabled-items)
  (display
   (make-instance 'actions-menu
                  :id "invoice-actions"
                  :style "hnavbar actions"
                  :spec (crud-actions-spec (apply #'invoice        invoice-kind :id id filter)
                                           (apply #'invoice/create invoice-kind filter)
                                           (apply #'invoice/update invoice-kind :id id filter)
                                           (apply #'invoice/delete invoice-kind :id id filter)))
   :disabled-items disabled-items))

(defun invoice-notifications ()
  (notifications
   '((company  (:company-title-unknown "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"))
     (amount (:non-positive-amount  "Το ποσό της συναλλαγής πρέπει να είναι θετικός αριθμός"
              :parse-error  "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες"))
     (account-id (:acc-id-null "Δεν έχετε επιλέξει λογαριασμό"))
     (date (:parse-error "Η ημερομηνία της συναλλαγής είναι άκυρη")))))



;;; ------------------------------------------------------------
;;; Pages
;;; ------------------------------------------------------------

(define-regex-page invoice (("financial/invoice/" (invoice-kind "receivable|payable")))
    ((search    string)
     (start     integer)
     (id        integer chk-tx-id))
  (with-auth ("configuration")
    (no-cache)
    (check-invoice-accounts)
    (if (every #'validp (parameters *page*))
        (let* ((filter (parameters->plist search))
               (page-title (conc "Τιμολόγια » " (invoice-kind-label invoice-kind) " » Κατάλογος"))
               (invoice-tx-table (make-instance 'invoice-tx-table
                                                :id "invoice-tx-table"
                                                :subpage invoice-kind
                                                :op 'catalogue
                                                :filter filter)))
          (with-document ()
            (:head
             (:title (str page-title))
             (financial-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'financial)
                   (financial-navbar 'invoice)
                   (:div :class "window grid_10"
                         (:div :class "title" (str page-title))
                         (invoice-menu invoice-kind
                                       (val id)
                                       filter
                                       (if (val id)
                                           '(catalogue)
                                           '(catalogue details update delete)))
                         (display invoice-tx-table
                                  :selected-id (val* id)
                                  :selected-data nil
                                  :start (val* start)))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (invoice invoice-kind) (val search))
                         (invoice-filters invoice-kind (val search)))
                   (footer)))))
        (see-other (notfound)))))

(define-regex-page invoice/create (("financial/invoice/" (invoice-kind  "receivable|payable") "/create"))
    ((search      string)
     (date        date)
     (company     string chk-company-title*)
     (description string)
     (amount      float   chk-amount)
     (account-id  integer chk-acc-id))
  (with-auth ("configuration")
    (no-cache)
    (check-invoice-accounts)
    (let ((filter (parameters->plist search))
          (page-title (conc "Τιμολόγια » " (invoice-kind-label invoice-kind) " » Δημιουργία")))
      (with-document ()
        (:head
         (:title (str page-title))
         (financial-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'financial)
               (financial-navbar 'invoice)
               (:div :id "invoice-window" :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (invoice-menu invoice-kind
                                   nil
                                   filter
                                   '(create update delete))
                     (invoice-notifications)
                     (with-form (actions/financial/invoice/create invoice-kind :search (val* search))
                       (invoice-data-form 'create invoice-kind
                                          :filter filter
                                          :data (parameters->plist date
                                                                   company
                                                                   description
                                                                   amount
                                                                   account-id)
                                          :styles (parameters->styles date
                                                                      company
                                                                      description
                                                                      amount))))
               (footer)))))))

(define-regex-page invoice/update (("financial/invoice/" (invoice-kind "receivable|payable") "/update"))
    ((search      string)
     (id          integer chk-tx-id t)
     (date        date)
     (company     string chk-company-title*)
     (description string)
     (amount      float   chk-amount)
     (account-id  integer chk-acc-id))
  (with-auth ("configuration")
    (no-cache)
    (check-invoice-accounts)
    (let ((filter (parameters->plist search))
          (page-title (conc "Τιμολόγια » " (invoice-kind-label invoice-kind) " » Επεξεργασία"))
          (acc-keyword (if (string-equal invoice-kind "payable") :debit-acc-id :credit-acc-id)))
      (with-document ()
        (:head
         (:title (str page-title))
         (financial-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'financial)
               (financial-navbar 'invoice)
               (:div :id "invoice-window" :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (invoice-menu invoice-kind
                                   nil
                                   filter
                                   '(create update delete))
                     (with-form (actions/financial/invoice/update invoice-kind
                                                                  :id (val id)
                                                                  :search (val* search))
                       (invoice-data-form 'update invoice-kind
                                          :id (val id)
                                          :filter filter
                                          :data (plist-union (parameters->plist date
                                                                                company
                                                                                description
                                                                                amount)
                                                             (subst :account-id
                                                                    acc-keyword
                                                                    (get-tx-plist (val id))))
                                          :styles (parameters->styles date
                                                                      company
                                                                      description
                                                                      amount
                                                                      account-id))))
               (footer)))))))

(define-regex-page invoice/delete (("financial/invoice/" (invoice-kind "receivable|payable") "/delete"))
    ((id     integer chk-project-id t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (check-invoice-accounts)
    (if (validp id)
        (let* ((filter (parameters->plist search))
               (page-title (conc "Τιμολόγια » " (invoice-kind-label invoice-kind) " » Διαγραφή"))
               (invoice-tx-table (make-instance 'invoice-tx-table
                                                :op 'delete
                                                :subpage invoice-kind
                                                :filter filter)))
          (with-document ()
            (:head
             (:title (str page-title))
             (financial-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'financial)
                   (financial-navbar 'invoice)
                   (:div :id "invoice-window" :class "window grid_10"
                         (:div :class "title" (str page-title))
                         (invoice-menu invoice-kind
                                       (val id)
                                       filter
                                       '(catalogue delete))
                         (with-form (actions/financial/invoice/delete invoice-kind
                                                                      :id (val id)
                                                                      :search (val* search))
                           (display invoice-tx-table
                                    :selected-id (val id))))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (invoice invoice-kind) (val search))
                         (invoice-filters invoice-kind (val search)))
                   (footer)))))
        (see-other (error-page)))))

(defun invoice-data-form (op invoice-kind &key id data styles filter)
  (let ((disabledp (eql op 'details))
        (tree (account-tree (string-equal invoice-kind "receivable"))))
    (flet ((label+textbox (name label &optional extra-styles)
             (with-html
               (label name label)
               (textbox name
                        :value (getf data (make-keyword name))
                        :disabledp disabledp
                        :style (conc (getf styles (make-keyword name))
                                     " " extra-styles)))))
      (with-html
        (:div :id "invoice-data-form" :class "data-form"
              (:div :class "grid_6 alpha"
                    (label+textbox 'date "Ημερομηνία" "datepicker")
                    (label+textbox 'description "Περιγραφή")
                    (label+textbox 'company "Εταιρία" "ac-company")
                    (label+textbox 'amount "Ποσό")
                    (:div :class "data-form-buttons"
                          (if disabledp
                              (cancel-button (apply #'invoice invoice-kind :id id filter)
                                             "Επιστροφή στον Κατάλογο Συναλλαγών Μετρητών")
                              (progn
                                (ok-button (if (eql op 'update) "Ανανέωση" "Δημιουργία"))
                                (cancel-button (apply #'invoice invoice-kind :id id filter)
                                               "Άκυρο")))))
              (:div :class "grid_6 omega"
                    (label 'account "Λογαριασμός")
                    ;; Display the tree. If needed, preselect the first account of the tree.
                    (display tree :selected-id (or (getf data :account-id)
                                                   (key (first (children (root tree))))))))))))