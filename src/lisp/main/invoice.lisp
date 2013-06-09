(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass invoice-family (tx-family)
  ()
  (:default-initargs
   :parameter-groups '(:system (tx-id)
                       :payload (tx-date company description amount account-id project-id)
                       :filter (search since until))))

(defclass invoice-page (auth-regex-page invoice-family)
  ((messages
    :allocation :class
    :reader messages
    :initform
    '((company
       (:company-title-unknown
        "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"
        :company-title-null
        "Η επωνυμία της εταιρίας είναι κενή"
        :company-immediate-tx-only
        "Επιτρέπονται μόνο συναλλαγές απ' ευθείας εξόφλησης (όχι έναντι ανοιχτού λογαριασμού) με αυτή την εταιρία"
        :company-supplier-only
        "Αυτή η εταιρία δεν μπορεί να εμφανίζει έσοδα."
        :company-customer-only
        "Αυτή η εταιρία δεν μπορεί να εμφανίζει έξοδα."))
      (amount
       (:empty-amount
        "Το ποσό της συναλλαγής είναι κενό"
        :non-positive-amount
        "Το ποσό της συναλλαγής δεν είναι θετικός αριθμός"
        :parse-error
        "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες"))
      (account-id
       (:account-id-null
        "Δεν έχετε επιλέξει λογαριασμό"))
      (project-id
       (:project-id-null
        "Δεν έχετε επιλέξει έργο."))
      (tx-date
       (:parse-error
        "Η ημερομηνία της συναλλαγής είναι άκυρη"))))))



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(defun check-invoice-accounts ()
  (unless (every #'integerp (list (account-id 'receivable-root-account)
                                  (account-id 'payable-root-account)
                                  (account-id 'revenues-root-account)
                                  (account-id 'expenses-root-account)))
    (see-other (invoice-accounts-error-page))))

(defpage dynamic-page invoice-accounts-error-page ("invoice/error")
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
;;; Invoice transactions table
;;; ----------------------------------------------------------------------

;;; table

(defclass invoice-tx-table (tx-table)
  ((kind :accessor kind :initarg :kind)
   (role :accessor role :initarg :role))
  (:default-initargs :item-class 'invoice-tx-row
                     :id "invoice-tx-table"
                     :paginator (make-instance 'invoice-paginator)
                     :header-labels '("" "Ημερομηνία" "Εταιρία" "Περιγραφή"
                                      "Λογαριασμός" "Ποσό" "" "")))

(defmethod get-records ((table invoice-tx-table))
  (labels ((invoice-receivable/payable-account (kind)
             (if (debit-invoice-p kind)
                 'tx.debit-account-id
                 'tx.credit-account-id))
           (invoice-revenues/expenses-account (kind)
             (if (debit-invoice-p kind)
                 'tx.credit-account-id
                 'tx.debit-account-id))
           (invoice-base-where (role kind)
             `((:= ,(invoice-receivable/payable-account kind)
                   ,(receivable/payable-root role))
               (:in ,(invoice-revenues/expenses-account kind)
                    (:set ,@(revenues/expenses-set role))))))
    (let* ((search (getf (filter table) :search))
           (since (getf (filter table) :since))
           (until (getf (filter table) :until))
           (kind (kind table))
           (role (role table))
           (base-query `(:select tx.id tx-date
                                 (:as company.title company)
                                 (:as company.id 'company-id)
                                 (:as account.title account)
                                 description amount
                         :from tx
                         :inner-join temtx
                         :on (:= tx.temtx-id temtx.id)
                         :inner-join company
                         :on (:= tx.company-id company.id)
                         :inner-join account
                         :on (:= account.id ,(invoice-revenues/expenses-account kind))))
           (where nil))
      (when search
        (push `(:or (:ilike description ,(ilike search))
                    (:ilike company.title ,(ilike search))
                    (:ilike account.title ,(ilike search)))
              where))
      (when (and since (not (eql since :null)))
        (push `(:<= ,since tx-date)
              where))
      (when (and until (not (eql until :null)))
        (push `(:<= tx-date ,until)
              where))
      (let ((sql `(:order-by
                   (,@base-query :where
                                 (:and ,@(invoice-base-where role kind)
                                       (:= temtx.lib-p nil)
                                       ,@where))
                   (:desc tx-date) account company description)))
        (query (sql-compile sql)
               :plists)))))

(defmethod actions ((tbl invoice-tx-table) &key)
  (let* ((tx-id (selected-key tbl))
         (kind (kind tbl))
         (role (role tbl))
         (filter (filter tbl))
         (hrefs (if tx-id
                    (list :details (apply #'invoice/details role kind :tx-id tx-id filter)
                          :journal (list (tx :tx-id tx-id) "Καθολικό" "journal")
                          :delete (apply #'invoice/delete role kind :tx-id tx-id filter))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))

(defmethod filters ((tbl invoice-tx-table))
  (let ((role (role tbl))
        (kind (kind tbl))
        (filter (filter tbl)))
    (filter-area (filter-navbar `((customer ,(apply #'invoice "customer" "debit" filter)
                                            "Πελάτες")
                                  (supplier ,(apply #'invoice "supplier" "credit" filter)
                                            "Προμηθευτές"))
                                :active role
                                :id "role-navbar")
                 (filter-navbar `((debit ,(apply #'invoice role "debit" filter)
                                         "Χρεώσεις")
                                  (credit ,(apply #'invoice role "credit" filter)
                                          "Πιστώσεις"))
                                :active kind
                                :id "kind-navbar")
                 (datebox (lambda (&rest args)
                            (apply #'invoice role kind args))
                          filter))))

;;; rows

(defclass invoice-tx-row (tx-row)
  ())

(defmethod selector ((row invoice-tx-row) selected-p)
  (let* ((tx-id (key row))
         (table (collection row))
         (pg (paginator table))
         (filter (filter table))
         (kind (kind table))
         (role (role table))
         (start (start-index table)))
    (html ()
      (:a :href (if selected-p
                    (apply #'invoice role kind :start (page-start pg (index row) start) filter)
                    (apply #'invoice role kind :tx-id tx-id filter))
        (selector-img selected-p)))))

(defmethod payload ((row invoice-tx-row) enabled-p)
  (let ((record (record row))
        (table (collection row)))
    (mapcar (textbox-maker record enabled-p)
            `(tx-date
              (company :href ,(company/tx :company-id (getf record :company-id)
                                          :tx-id (key row)))
              (description :href ,(apply #'invoice/details (role table) (kind table)
                                         :tx-id (key row) (filter table)))
              account
              (amount :format-fn ,#'fmt-amount)))))

(defmethod controls ((row invoice-tx-row) controls-p)
  (let* ((tx-id (key row))
         (table (collection row))
         (filter (filter table))
         (kind (kind table))
         (role (role table)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button :href (apply #'invoice role kind :tx-id tx-id filter)))
        (list nil nil))))


;;; paginator

(defclass invoice-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg invoice-paginator) start)
  (let ((table (table pg)))
    (apply #'invoice (role table) (kind table) :start start (filter table))))



;;; ----------------------------------------------------------------------
;;; Utilities
;;; ----------------------------------------------------------------------

(defun debit-invoice-p (kind)
  (string-equal kind "debit"))

(defun invoice-debit-account-id (role kind account-id)
  (if (debit-invoice-p kind)
      (receivable/payable-root role)
      account-id))

(defun invoice-credit-account-id (role kind account-id)
  (if (debit-invoice-p kind)
      account-id
      (receivable/payable-root role)))

(defun invoice-page-title (role kind op-label)
  (let ((kind-label
          (if (debit-invoice-p kind) "Χρεώσεις" "Πιστώσεις"))
        (role-label
          (if (customer-p role) "Πελάτες" "Προμηθευτές")))
    (conc role-label " » " kind-label " » " op-label)))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun invoice-top-actions (op)
  (let* ((kind (second *registers*))
         (new-invoice-label (conc "Νέα " (if (debit-invoice-p kind) "Χρέωση" "Πίστωση"))))
    (top-actions-area
     (make-instance 'scrooge-menu
                    :spec (make-menu-spec
                           `(:catalogue ,(family-url 'invoice :system :filter)
                             :create (,(family-url 'invoice/create :filter) ,new-invoice-label)))
                    :css-class "hmenu"
                    :disabled (cond ((member op '(:catalogue :delete))
                                     '(:catalogue))
                                    ((eql op :create)
                                     '(:create))
                                    (t
                                     nil)))
     (searchbox (family-url-fn 'actions/invoice/search)
                (family-url-fn 'invoice :system)
                (family-params 'invoice :filter)
                "ac-company"))))



;;; ----------------------------------------------------------------------
;;; Invoice form
;;; ----------------------------------------------------------------------

(defclass invoice-form (tx-form)
  ((role :accessor role :initarg :role)
   (kind :accessor kind :initarg :kind)))

(defmethod display ((form invoice-form) &key styles)
  (let* ((role (role form))
         (kind (kind form))
         (customer-p (customer-p role))
         (disabled (eql (op form) :details))
         (record (record form))
         (root-key (revenues/expenses-root role))
         (tree (make-instance 'radio-account-tree
                              :css-class "crud-tree company-dependent"
                              :disabled disabled
                              :root-key root-key
                              :debit-p (not customer-p)
                              :selected-key (or (getf record :account-id)
                                                (getf record (if (debit-invoice-p kind)
                                                                 :credit-account-id
                                                                 :debit-account-id))
                                                root-key))))
    (with-html
      (:div :id "split-data-form" :class "data-form"
        (:div :class "grid_6 alpha"
          (left-column form styles disabled))
        (:div :class "grid_5 omega"
          (:h3 (str (conc "Λογαριασμός " (if (debit-invoice-p kind) "χρέωσης" "πίστωσης"))))
          ;; Display the tree. If needed, preselect the first account of the tree.
          (display tree))
        (clear)))))

(defmethod actions ((form invoice-form) &key filter)
  (let* ((tx-id (key form))
         (role (role form))
         (kind (kind form))
         (hrefs (list :update (apply #'invoice/update role kind :tx-id tx-id filter)
                      :journal (list (tx :tx-id tx-id) "Καθολικό" "journal")
                      :delete (apply #'invoice/delete role kind :tx-id tx-id filter))))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions form))))



;;; ----------------------------------------------------------------------
;;; SEARCH
;;; ----------------------------------------------------------------------

(defpage invoice-page actions/invoice/search
    (("actions/invoice/" (role "(customer|supplier)") "/" (kind "(debit|credit)") "/search")
     :request-type :get)
    ((search string)
     (since  string)
     (until  string))
  (with-db ()
    (let* ((filter (params->filter))
           (records (records (make-instance 'invoice-tx-table :op :catalogue
                                                              :role role
                                                              :kind kind
                                                              :filter filter))))
      (if (single-item-list-p records)
          (see-other (apply #'invoice/details role kind
                            :tx-id (get-key (first records))
                            filter))
          (see-other (apply #'invoice role kind filter))))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defun invoice-kind (record)
  (if (or (member (getf record :debit-account-id) *expense-accounts*)
          (member (getf record :credit-account-id) *revenue-accounts*))
      "debit"
      "credit"))

(defpage invoice-page invoice (("invoice/"
                                (role "(customer|supplier)") "/"
                                (kind "(debit|credit)")))
    ((tx-id  integer chk-tx-id)
     (start  integer)
     (search string)
     (since  date)
     (until  date))
  (check-invoice-accounts)
  (with-view-page
    (let* ((filter (params->filter))
           (page-title (invoice-page-title role kind "Κατάλογος"))
           (invoice-tx-table (make-instance 'invoice-tx-table :op :catalogue
                                                              :role role
                                                              :kind kind
                                                              :selected-key (val tx-id)
                                                              :filter filter
                                                              :start-index (val start))))
      (check-id-inclusion invoice-tx-table
                          (val tx-id)
                          (apply #'invoice role kind filter))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (navbar 'main 'invoice)
            (invoice-top-actions :catalogue)
            (filters invoice-tx-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" (str page-title))
                (actions invoice-tx-table)
                (display invoice-tx-table)))
            (footer)))))))

(defpage invoice-page invoice/details (("invoice/"
                                        (role "(customer|supplier)") "/"
                                        (kind "(debit|credit)")
                                        "/details"))
    ((tx-id  integer chk-tx-id t)
     (search string)
     (since  date)
     (until  date))
  (with-view-page
    (let* ((filter (params->filter))
           (invoice-form (make-instance 'invoice-form
                                        :role role
                                        :kind kind
                                        :op :details
                                        :key (val tx-id)))
           (page-title (invoice-page-title role kind "Λεπτομέρειες")))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (navbar 'main 'invoice)
            (invoice-top-actions :details)
            (:div :class "grid_12"
              (:div :id "invoice-window" :class "window"
                (:p :class "title" "Λεπτομέρειες")
                (actions invoice-form :filter filter)
                (display invoice-form)))
            (footer)))))))


;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage invoice-page invoice/create
    (("invoice/" (role "(customer|supplier)") "/" (kind "(debit|credit)") "/create"))
    ((tx-date     date)
     (description string)
     (company     string  chk-company-title/cash)
     (amount      float   chk-amount)
     (account-id  integer chk-account-id)
     (search      string)
     (since       date)
     (until       date)
     (project-id  integer chk-project-id))
  (validate-parameters (tx-company-constraints-chker role) company)
  (check-invoice-accounts)
  (with-view-page
    (let* ((filter (params->filter))
           (invoice-form (make-instance 'invoice-form
                                        :role role
                                        :kind kind
                                        :op :create
                                        :cancel-url (apply #'invoice role kind filter)))
           (page-title (invoice-page-title role kind "Δημιουργία")))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (navbar 'main 'invoice)
            (invoice-top-actions :create)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" (str page-title))
                (actions invoice-form :filter filter)
                (notifications)
                (with-form (actions/invoice/create role kind
                                                   :search (val search)
                                                   :since (val since)
                                                   :until (val until))
                  (display invoice-form :payload (params->payload)
                                        :styles (params->styles)))))
            (footer)))))))

(defpage invoice-page actions/invoice/create
    (("actions/invoice/" (role "(customer|supplier)") "/" (kind "(debit|credit)") "/create")
     :request-type :post)
    ((tx-date     date)
     (description string)
     (company     string  chk-company-title/cash t)
     (amount      float   chk-amount             t)
     (account-id  integer chk-account-id         t)
     (search      string)
     (since       date)
     (until       date)
     (project-id  integer chk-project-id))
  (validate-parameters (tx-company-constraints-chker role) company)
  (check-invoice-accounts)
  (with-controller-page (invoice/create role kind)
    (let* ((company-id (company-id (val company)))
           (debit-account-id (invoice-debit-account-id role kind (val account-id)))
           (credit-account-id (invoice-credit-account-id role kind (val account-id)))
           (new-tx (make-instance 'tx
                                  :tx-date (val tx-date)
                                  :description (val description)
                                  :company-id company-id
                                  :amount (val amount)
                                  :credit-account-id credit-account-id
                                  :debit-account-id debit-account-id)))
      (insert-dao new-tx)
      (see-other (apply #'invoice/details role kind :tx-id (tx-id new-tx)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage invoice-page invoice/update
    (("invoice/" (role "(customer|supplier)") "/" (kind "(debit|credit)") "/update"))
    ((tx-id       integer chk-tx-id         t)
     (tx-date     date)
     (description string)
     (company     string  chk-company-title/cash)
     (amount      float   chk-amount)
     (account-id  integer chk-account-id)
     (search      string)
     (since       date)
     (until       date)
     (project-id  integer chk-project-id))
  (validate-parameters (tx-company-constraints-chker role) company)
  (check-invoice-accounts)
  (with-view-page
    (let* ((filter (params->filter))
           (invoice-form (make-instance 'invoice-form
                                        :role role
                                        :kind kind
                                        :op :update
                                        :key (val tx-id)
                                        :cancel-url (apply #'invoice/details role kind
                                                           :tx-id (val tx-id)
                                                           filter)))
           (page-title (invoice-page-title role kind "Επεξεργασία")))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (navbar 'main 'invoice)
            (invoice-top-actions :update)
            (:div :class "grid_12"
              (:div :id "invoice-window" :class "window"
                (:p :class "title" "Επεξεργασία")
                (actions invoice-form :filter filter)
                (notifications)
                (with-form (actions/invoice/update role kind
                                                   :tx-id (val tx-id)
                                                   :search (val search)
                                                   :since (val since)
                                                   :until (val until))
                  (display invoice-form :payload (params->payload)
                                        :styles (params->styles)))))
            (footer)))))))

(defpage invoice-page actions/invoice/update
    (("actions/invoice/" (role "(customer|supplier)") "/" (kind "(debit|credit)") "/update")
     :request-type :post)
    ((tx-id       integer chk-tx-id         t)
     (tx-date     date)
     (description string)
     (company     string  chk-company-title/cash)
     (amount      float   chk-amount)
     (account-id  integer chk-account-id)
     (search      string)
     (since       date)
     (until       date)
     (project-id  integer chk-project-id))
  (validate-parameters (tx-company-constraints-chker role) company)
  (check-invoice-accounts)
  (with-controller-page (invoice/update role kind)
    (let ((company-id (company-id (val company)))
          (debit-account-id (invoice-debit-account-id role kind (val account-id)))
          (credit-account-id (invoice-credit-account-id role kind (val account-id))))
      (execute (:update 'tx :set
                        'tx-date (val tx-date)
                        'description (val description)
                        'company-id company-id
                        'amount (val amount)
                        'debit-account-id debit-account-id
                        'credit-account-id credit-account-id
                        :where (:= 'id (val tx-id))))
      (see-other (apply #'invoice/details role kind :tx-id (val tx-id)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage invoice-page invoice/delete
    (("invoice/" (role "(customer|supplier)") "/" (kind "(debit|credit)") "/delete"))
    ((tx-id  integer chk-tx-id/ref t)
     (search string)
     (since  date)
     (until  date))
  (check-invoice-accounts)
  (with-view-page
    (let* ((filter (params->filter))
           (page-title (invoice-page-title role kind "Διαγραφή"))
           (invoice-tx-table (make-instance 'invoice-tx-table
                                            :role role
                                            :kind kind
                                            :op :delete
                                            :selected-key (val tx-id)
                                            :filter filter)))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (navbar 'main 'invoice)
            (invoice-top-actions :delete)
            (filters invoice-tx-table)
            (:div :class "grid_12"
              (:div :id "invoice-window" :class "window"
                (:div :class "title" (str page-title))
                (actions invoice-tx-table)
                (with-form (actions/invoice/delete role kind
                                                   :tx-id (val tx-id)
                                                   :search (val search)
                                                   :since (val since)
                                                   :until (val until))
                  (display invoice-tx-table))))
            (footer)))))))

(defpage invoice-page actions/invoice/delete
    (("actions/invoice/" (role "(customer|supplier)") "/" (kind "(debit|credit)") "/delete")
     :request-type :post)
    ((tx-id  integer chk-tx-id/ref t)
     (search string)
     (since  date)
     (until  date))
  (check-invoice-accounts)
  (with-controller-page (invoice/delete role kind)
    (delete-dao (get-dao 'tx (val tx-id)))
    (see-other (apply #'invoice role kind
                      (params->filter)))))
