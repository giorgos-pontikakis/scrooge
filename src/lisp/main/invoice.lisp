(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass invoice-family (tx-family)
  ()
  (:default-initargs
   :parameter-groups '(:system (tx-id)
                       :payload (tx-date company description amount account-id)
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
        :company-cash-only
        "Επιτρέπονται μόνο συναλλαγές μετρητών με αυτή την εταιρία"))
      (amount
       (:empty-amount
        "Το ποσό της συναλλαγής είναι κενό"
        :non-positive-amount
        "Το ποσό της συναλλαγής δεν είναι θετικός αριθμός"
        :parse-error
        "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες"))
      (account-id
       (:acc-id-null
        "Δεν έχετε επιλέξει λογαριασμό"))
      (tx-date
       (:parse-error
        "Η ημερομηνία της συναλλαγής είναι άκυρη"))))))



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(defun check-invoice-accounts ()
  (unless (every #'integerp (list (account-id 'invoice-receivable-account)
                                  (account-id 'invoice-payable-account)
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
  ((header-labels  :initform '("" "Ημερομηνία" "Εταιρία" "Περιγραφή" "Λογαριασμός" "Ποσό" "" ""))
   (kind :accessor kind :initarg :kind)
   (issuer :accessor issuer :initarg :issuer)
   (paginator :initform (make-instance 'invoice-paginator
                                       :css-class "paginator")))
  (:default-initargs :item-class 'invoice-tx-row :id "invoice-tx-table"))

(defmethod get-records ((table invoice-tx-table))
  (labels ((invoice-receivable/payable-account (kind)
             (if (invoice-debit-p kind)
                 'tx.debit-acc-id
                 'tx.credit-acc-id))
           (invoice-revenues/expenses-account (kind)
             (if (invoice-debit-p kind)
                 'tx.credit-acc-id
                 'tx.debit-acc-id))
           (invoice-base-where (issuer kind)
             `((:= ,(invoice-receivable/payable-account kind)
                   ,(invoice-receivable/payable-root issuer))
               (:in ,(invoice-revenues/expenses-account kind)
                    (:set ,@(invoice-revenues/expenses-set issuer))))))
    (let* ((search (getf (filter table) :search))
           (since (getf (filter table) :since))
           (until (getf (filter table) :until))
           (kind (kind table))
           (issuer (issuer table))
           (base-query `(:select tx.id tx-date
                                 (:as company.title company)
                                 (:as company.id 'company-id)
                                 (:as account.title account)
                                 description amount
                         :from tx
                         :inner-join company
                         :on (:= tx.company-id company.id)
                         :inner-join account
                         :on (:= account.id ,(invoice-revenues/expenses-account kind))))
           (where nil))
      (when search
        (push `(:or (:ilike description ,(ilike search))
                    (:ilike company.title ,(ilike search)))
              where))
      (when (and since (not (eql since :null)))
        (push `(:<= ,since tx-date)
              where))
      (when (and until (not (eql until :null)))
        (push `(:<= tx-date ,until)
              where))
      (let ((sql `(:order-by
                   (,@base-query :where
                                 (:and ,@(invoice-base-where issuer kind)
                                       ,@where))
                   (:desc tx-date) account company description)))
        (query (sql-compile sql)
               :plists)))))

(defmethod actions ((tbl invoice-tx-table) &key)
  (let* ((tx-id (selected-key tbl))
         (kind (kind tbl))
         (issuer (issuer tbl))
         (filter (filter tbl))
         (hrefs (if tx-id
                    (list :details (apply #'invoice/details issuer kind :tx-id tx-id filter)
                          :delete (apply #'invoice/delete issuer kind :tx-id tx-id filter))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))

(defmethod filters ((tbl invoice-tx-table))
  (let ((issuer (issuer tbl))
        (kind (kind tbl))
        (filter (filter tbl)))
    (filter-area (filter-navbar `((customer ,(apply #'invoice "customer" "debit" filter)
                                            "Πελάτες")
                                  (supplier ,(apply #'invoice "supplier" "credit" filter)
                                            "Προμηθευτές"))
                                :active issuer
                                :id "issuer-navbar")
                 (filter-navbar `((debit ,(apply #'invoice issuer "debit" filter)
                                         "Χρεώσεις")
                                  (credit ,(apply #'invoice issuer "credit" filter)
                                          "Πιστώσεις"))
                                :active kind
                                :id "kind-navbar")
                 (datebox (lambda (&rest args)
                            (apply #'invoice issuer kind args))
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
         (issuer (issuer table))
         (start (start-index table)))
    (html ()
      (:a :href (if selected-p
                    (apply #'invoice issuer kind :start (page-start pg (index row) start) filter)
                    (apply #'invoice issuer kind :tx-id tx-id filter))
          (selector-img selected-p)))))

(defmethod payload ((row invoice-tx-row) enabled-p)
  (let ((record (record row)))
    (insert-list 1
                 (html ()
                   (:a :href (company/details :company-id (getf record :company-id))
                       (str (getf record :company))))
                 (append (mapcar (lambda (name)
                                   (make-instance 'textbox
                                                  :name name
                                                  :value (getf record (make-keyword name))
                                                  :disabled (not enabled-p)))
                                 '(tx-date description account))
                         (list
                          (make-instance 'textbox
                                         :name 'amount
                                         :value (fmt-amount (getf record :amount))
                                         :disabled (not enabled-p)))))))

(defmethod controls ((row invoice-tx-row) controls-p)
  (let* ((tx-id (key row))
         (table (collection row))
         (filter (filter table))
         (kind (kind table))
         (issuer (issuer table)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button :href (apply #'invoice issuer kind :tx-id tx-id filter)))
        (list nil nil))))


;;; paginator

(defclass invoice-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg invoice-paginator) start)
  (let ((table (table pg)))
   (apply #'invoice (issuer table) (kind table) :start start (filter table))))



;;; ----------------------------------------------------------------------
;;; Utilities
;;; ----------------------------------------------------------------------

(defun invoice-customer-p (issuer)
  (string-equal issuer "customer"))

(defun invoice-debit-p (kind)
  (string-equal kind "debit"))

(defun invoice-revenues/expenses-set (issuer)
  (if (invoice-customer-p issuer) *revenues-accounts* *expense-accounts*))

(defun invoice-revenues/expenses-root (issuer)
  (if (invoice-customer-p issuer) (account-id 'revenues-root-account) (account-id 'expenses-root-account)))

(defun invoice-receivable/payable-root (issuer)
  (if (invoice-customer-p issuer) (account-id 'invoice-receivable-account) (account-id 'invoice-payable-account)))

(defun invoice-debit-acc-id (issuer kind account-id)
  (if (invoice-debit-p kind)
      (invoice-receivable/payable-root issuer)
      account-id))

(defun invoice-credit-acc-id (issuer kind account-id)
  (if (invoice-debit-p kind)
      account-id
      (invoice-receivable/payable-root issuer)))

(defun invoice-page-title (issuer kind op-label)
  (let ((kind-label
          (if (invoice-debit-p kind) "Χρεώσεις" "Πιστώσεις"))
        (issuer-label
          (if (invoice-customer-p issuer) "Πελάτες" "Προμηθευτές")))
    (conc issuer-label " » " kind-label " » " op-label)))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun invoice-top-actions (op)
  (let* ((kind (second *registers*))
         (new-invoice-label (conc "Νέα "
                                  (if (invoice-debit-p kind)
                                      "Χρέωση"
                                      "Πίστωση"))))
    (top-actions-area
     (make-instance 'scrooge-menu
                    :spec (make-menu-spec
                           `(:catalogue ,(family-url 'invoice :system :filter)
                             :create (,(family-url 'invoice/create :filter) ,new-invoice-label)))
                    :css-class "hmenu"
                    :disabled (cond ((member op '(:catalogue :delete))
                                     '(catalogue))
                                    ((eql op :create)
                                     '(create))
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
  ((issuer :accessor issuer :initarg :issuer)))

(defmethod display ((form invoice-form) &key styles)
  (let* ((issuer (issuer form))
         (kind (kind form))
         (customer-p (invoice-customer-p issuer))
         (disabled (eql (op form) :details))
         (record (record form))
         (ldfn (label-datum disabled record styles))
         (root-key (invoice-revenues/expenses-root issuer))
         (tree (make-instance 'rev/exp-account-tree
                              :css-class "crud-tree company-dependent"
                              :disabled disabled
                              :root-key root-key
                              :debit-p (not (invoice-customer-p issuer))
                              :selected-key (or (getf record :account-id)
                                                (getf record (if (invoice-debit-p kind)
                                                                 :credit-acc-id
                                                                 :debit-acc-id))
                                                root-key))))
    (with-html
      (:div :id "invoice-data-form" :class "data-form"
            (:div :class "grid_5 prefix_1 alpha"
                  (display ldfn 'tx-date "Ημερομηνία" :enabled-styles "datepicker"
                                                     :default-value (today))
                  (display ldfn 'description "Περιγραφή")
                  (display ldfn 'company "Εταιρία"
                           :enabled-styles "ac-company"
                           :href (company/details :company-id (getf record :company-id)))
                  (display ldfn 'amount "Ποσό")
                  (unless disabled
                    (htm (:div :class "data-form-buttons"
                               (ok-button :body (if (eql (op form) :update)
                                                    "Ανανέωση"
                                                    "Δημιουργία"))
                               (cancel-button (cancel-url form)
                                              :body "Άκυρο")))))
            (:div :class "grid_5 omega"
                  (label 'account-id (conc "Λογαριασμός "
                                           (if customer-p "εσόδων" "εξόδων")))
                  ;; Display the tree. If needed, preselect the first account of the tree.
                  (display tree))
            (clear)))))

(defmethod actions ((form invoice-form) &key filter)
  (let* ((tx-id (key form))
         (issuer (issuer form))
         (kind (kind form))
         (hrefs (list :update (apply #'invoice/update issuer kind :tx-id tx-id filter)
                      :delete (apply #'invoice/delete issuer kind :tx-id tx-id filter))))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions form))))



;;; ----------------------------------------------------------------------
;;; SEARCH
;;; ----------------------------------------------------------------------

(defpage invoice-page actions/invoice/search
    (("actions/invoice/" (issuer "(customer|supplier)") "/" (kind "(debit|credit)") "/search")
     :request-type :get)
    ((search string)
     (since  string)
     (until  string))
  (with-db ()
    (let* ((filter (params->filter))
           (rows (rows (make-instance 'invoice-tx-table
                                      :issuer issuer
                                      :kind kind
                                      :filter filter))))
      (if (single-item-list-p rows)
          (see-other (apply #'invoice/details issuer kind
                            :tx-id (key (first rows))
                            filter))
          (see-other (apply #'invoice issuer kind filter))))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defun invoice-kind (dao)
  (if (or (member (debit-acc-id dao) *revenues-accounts*)
          (member (debit-acc-id dao) *expense-accounts*))
      "debit"
      "credit"))

(defun invoice-issuer (dao)
  (if (or (member (debit-acc-id dao) *revenues-accounts*)
          (member (credit-acc-id dao) *revenues-accounts*))
      "customer"
      "supplier"))

(defpage invoice-page invoice (("invoice/"
                                (issuer "(customer|supplier)") "/"
                                (kind "(debit|credit)")))
    ((tx-id  integer chk-tx-id)
     (start  integer)
     (search string)
     (since  date)
     (until  date))
  (with-view-page
    (check-invoice-accounts)
    (let* ((filter (params->filter))
           (page-title (invoice-page-title issuer kind "Κατάλογος"))
           (invoice-tx-table (make-instance 'invoice-tx-table
                                            :issuer issuer
                                            :op :catalogue
                                            :kind kind
                                            :selected-key (val tx-id)
                                            :filter filter
                                            :start-index (val start))))
      ;; if tx-id exists and is not found among records, ignore search term
      (when (and (val tx-id)
                 (not (find (val tx-id) (rows invoice-tx-table) :key #'key)))
        (let ((tx (get-dao 'tx (val tx-id))))
          (see-other (invoice (invoice-issuer tx) (invoice-kind tx)
                              :tx-id (val tx-id)))))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'invoice)
               (invoice-top-actions :catalogue)
               (filters invoice-tx-table)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" (str page-title))
                           (actions invoice-tx-table)
                           (display invoice-tx-table)))
               (footer)))))))

(defpage invoice-page invoice/details (("invoice/"
                                        (issuer "(customer|supplier)") "/"
                                        (kind "(debit|credit)")
                                        "/details"))
    ((tx-id  integer chk-tx-id t)
     (search string)
     (since  date)
     (until  date))
  (with-view-page
    (let* ((filter (params->filter))
           (invoice-form (make-instance 'invoice-form
                                        :issuer issuer
                                        :kind kind
                                        :op :details
                                        :key (val tx-id)))
           (page-title (invoice-page-title issuer kind "Λεπτομέρειες")))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'invoice)
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
    (("invoice/" (issuer "(customer|supplier)") "/" (kind "(debit|credit)") "/create"))
    ((tx-date     date)
     (description string)
     (company     string  chk-company-title/cash)
     (amount      float   chk-amount)
     (account-id  integer chk-account-id)
     (search      string)
     (since       date)
     (until       date))
  (with-view-page
    (check-invoice-accounts)
    (let* ((filter (params->filter))
           (invoice-form (make-instance 'invoice-form
                                        :issuer issuer
                                        :kind kind
                                        :op :create
                                        :cancel-url (apply #'invoice issuer kind filter)))
           (page-title (invoice-page-title issuer kind "Δημιουργία")))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'invoice)
               (invoice-top-actions :create)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" (str page-title))
                           (actions invoice-form :filter filter)
                           (notifications)
                           (with-form (actions/invoice/create issuer kind
                                                              :search (val search)
                                                              :since (val since)
                                                              :until (val until))
                             (display invoice-form :payload (params->payload)
                                                   :styles (params->styles)))))
               (footer)))))))

(defpage invoice-page actions/invoice/create
    (("actions/invoice/" (issuer "(customer|supplier)") "/" (kind "(debit|credit)") "/create")
     :request-type :post)
    ((tx-date     date)
     (description string)
     (company     string  chk-company-title/cash t)
     (amount      float   chk-amount t)
     (account-id  integer chk-account-id t)
     (search      string)
     (since       date)
     (until       date))
  (with-controller-page (invoice/create issuer kind)
    (check-invoice-accounts)
    (let* ((company-id (company-id (val company)))
           (debit-acc-id (invoice-debit-acc-id issuer kind (val account-id)))
           (credit-acc-id (invoice-credit-acc-id issuer kind (val account-id)))
           (new-tx (make-instance 'tx
                                  :tx-date (val tx-date)
                                  :description (val description)
                                  :company-id company-id
                                  :amount (val amount)
                                  :credit-acc-id credit-acc-id
                                  :debit-acc-id debit-acc-id
                                  :auto t)))
      (insert-dao new-tx)
      (see-other (apply #'invoice/details issuer kind :tx-id (tx-id new-tx)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage invoice-page invoice/update
    (("invoice/" (issuer "(customer|supplier)") "/" (kind "(debit|credit)") "/update"))
    ((tx-id       integer chk-tx-id         t)
     (tx-date     date)
     (description string)
     (company     string  chk-company-title/cash)
     (amount      float   chk-amount)
     (account-id  integer chk-account-id)
     (search      string)
     (since       date)
     (until       date))
  (with-view-page
    (check-invoice-accounts)
    (let* ((filter (params->filter))
           (invoice-form (make-instance 'invoice-form
                                        :issuer issuer
                                        :kind kind
                                        :op :update
                                        :key (val tx-id)
                                        :cancel-url (apply #'invoice/details issuer kind
                                                           :tx-id (val tx-id)
                                                           filter)))
           (page-title (invoice-page-title issuer kind "Επεξεργασία")))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'invoice)
               (invoice-top-actions :update)
               (:div :class "grid_12"
                     (:div :id "invoice-window" :class "window"
                           (:p :class "title" "Επεξεργασία")
                           (actions invoice-form :filter filter)
                           (notifications)
                           (with-form (actions/invoice/update issuer kind
                                                              :tx-id (val tx-id)
                                                              :search (val search)
                                                              :since (val since)
                                                              :until (val until))
                             (display invoice-form :payload (params->payload)
                                                   :styles (params->styles)))))
               (footer)))))))

(defpage invoice-page actions/invoice/update
    (("actions/invoice/" (issuer "(customer|supplier)") "/" (kind "(debit|credit)") "/update")
     :request-type :post)
    ((tx-id       integer chk-tx-id         t)
     (tx-date     date)
     (description string)
     (company     string  chk-company-title/cash)
     (amount      float   chk-amount)
     (account-id  integer chk-account-id)
     (search      string)
     (since       date)
     (until       date))
  (with-controller-page (invoice/update issuer kind)
    (check-invoice-accounts)
    (let ((company-id (company-id (val company)))
          (debit-acc-id (invoice-debit-acc-id issuer kind (val account-id)))
          (credit-acc-id (invoice-credit-acc-id issuer kind (val account-id))))
      (execute (:update 'tx :set
                        'tx-date (val tx-date)
                        'description (val description)
                        'company-id company-id
                        'amount (val amount)
                        'debit-acc-id debit-acc-id
                        'credit-acc-id credit-acc-id
                        :where (:= 'id (val tx-id))))
      (see-other (apply #'invoice/details issuer kind :tx-id (val tx-id)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage invoice-page invoice/delete
    (("invoice/" (issuer "(customer|supplier)") "/" (kind "(debit|credit)") "/delete"))
    ((tx-id  integer chk-tx-id t)
     (search string)
     (since  date)
     (until  date))
  (with-view-page
    (check-invoice-accounts)
    (let* ((filter (params->filter))
           (page-title (invoice-page-title issuer kind "Διαγραφή"))
           (invoice-tx-table (make-instance 'invoice-tx-table
                                            :issuer issuer
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
               (header)
               (main-navbar 'invoice)
               (invoice-top-actions :delete)
               (filters invoice-tx-table)
               (:div :class "grid_12"
                     (:div :id "invoice-window" :class "window"
                           (:div :class "title" (str page-title))
                           (actions invoice-tx-table)
                           (with-form (actions/invoice/delete issuer kind
                                                              :tx-id (val tx-id)
                                                              :search (val search)
                                                              :since (val since)
                                                              :until (val until))
                             (display invoice-tx-table))))
               (footer)))))))

(defpage invoice-page actions/invoice/delete
    (("actions/invoice/" (issuer "(customer|supplier)") "/" (kind "(debit|credit)") "/delete")
     :request-type :post)
    ((tx-id  integer chk-tx-id t)
     (search string)
     (since  date)
     (until  date))
  (with-controller-page (invoice/delete issuer kind)
    (check-invoice-accounts)
    (delete-dao (get-dao 'tx (val tx-id)))
    (see-other (apply #'invoice issuer kind
                      (params->filter)))))
