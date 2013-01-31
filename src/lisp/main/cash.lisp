(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass cash-family (tx-family)
  ()
  (:default-initargs
   :parameter-groups '(:system (tx-id)
                       :payload (tx-date description company amount account-id)
                       :filter (search since until))))

(defclass cash-page (auth-regex-page cash-family)
  ((messages
    :allocation :class
    :reader messages
    :initform
    '((company
       (:company-title-unknown
        "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"
        :company-title-null
        "Η επωνυμία της εταιρίας είναι κενή"
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
      (tx-date
       (:parse-error
        "Η ημερομηνία της συναλλαγής είναι άκυρη"))))))



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(defun check-cash-accounts ()
  (unless (and (account-id 'cash-account)
               (account-id 'revenues-root-account)
               (account-id 'expenses-root-account))
    (see-other (cash-accounts-error-page))))

(defpage dynamic-page cash-accounts-error-page ("cash/error")
    ()
  (with-document ()
    (:head
      (:title "Cash accounts error")
      (error-headers))
    (:body
      (:div :id "header"
        (logo))
      (:div :id "body"
        (:div :id "content" :class "summary"
          (:p "Δεν έχετε ορίσει στις ρυθμίσεις είτε τον λογαριασμό μετρητών, είτε το λογαριασμό ρίζας εσόδων, ή το λογαριασμό ρίζας εξόδων."))))))



;;; ----------------------------------------------------------------------
;;; Cash transactions table
;;; ----------------------------------------------------------------------

;;; table

(defclass cash-tx-table (tx-table)
  ((role :accessor role
         :initarg :role)
   (paginator     :initform (make-instance 'cash-paginator
                                           :css-class "paginator"))
   (header-labels :initform '("" "Ημερομηνία" "Εταιρία" "Περιγραφή" "Λογαριασμός" "Ποσό" "" "")))
  (:default-initargs :item-class 'cash-tx-row :id "cash-tx-table"))

(defmethod get-records ((table cash-tx-table))
  (flet ((account-filter (role)
           (if (customer-p role)
               `(tx.debit-account-id ,(account-id 'cash-account))    ; revenue
               `(tx.credit-account-id ,(account-id 'cash-account)))) ; expense
         (account-join (role)
           (if (customer-p role)
               'tx.credit-account-id   ; revenue
               'tx.debit-account-id))) ; expense
    (let* ((search (getf (filter table) :search))
           (since (getf (filter table) :since))
           (until (getf (filter table) :until))
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
                         :on (:= ,(account-join role) account.id)
                         :left-join 'cheque-event
                         :on (:= 'cheque-event.tx-id 'tx.id)))
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
      (let ((sql `(:order-by (,@base-query :where
                                           (:and (:= ,@(account-filter role))
                                                 (:= temtx.lib-p nil)
                                                 (:is-null 'cheque-event.cheque-id)
                                                 ,@where))
                             (:desc tx-date) account company description)))
        (query (sql-compile sql)
               :plists)))))

(defmethod actions ((tbl cash-tx-table) &key)
  (let* ((tx-id (selected-key tbl))
         (role (role tbl))
         (filter (filter tbl))
         (hrefs (if tx-id
                    (list :details (apply #'cash/details role :tx-id tx-id filter)
                          :delete (apply #'cash/delete role :tx-id tx-id filter))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))

(defmethod filters ((tbl cash-tx-table))
  (let ((role (role tbl))
        (filter (filter tbl)))
    (filter-area (filter-navbar `((customer ,(apply #'cash "customer" filter) "Έσοδα")
                                  (supplier ,(apply #'cash "supplier" filter) "Έξοδα"))
                                :active role
                                :id "role-navbar")
                 (datebox (lambda (&rest args)
                            (apply #'cash role args))
                          filter))))



;;; rows

(defclass cash-tx-row (tx-row)
  ())

(defmethod selector ((row cash-tx-row) selected-p)
  (let* ((tx-id (key row))
         (table (collection row))
         (pg (paginator table))
         (filter (filter table))
         (role (role table))
         (start (start-index table)))
    (html ()
      (:a :href (if selected-p
                    (apply #'cash role :start (page-start pg (index row) start) filter)
                    (apply #'cash role :tx-id tx-id filter))
        (selector-img selected-p)))))

(defmethod payload ((row cash-tx-row) enabled-p)
  (let ((record (record row))
        (table (collection row)))
    (mapcar (textbox-maker record enabled-p)
            `(tx-date
              (company :href ,(company/tx :company-id (getf record :company-id)
                                          :tx-id (key row)))
              (description :href ,(apply #'cash/details (role table)
                                         :tx-id (key row) (filter table)))
              account
              (amount :format-fn ,#'fmt-amount)))))

(defmethod controls ((row cash-tx-row) controls-p)
  (let* ((tx-id (key row))
         (table (collection row))
         (filter (filter table))
         (role (role table)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button :href (apply #'cash role :tx-id tx-id filter)))
        (list nil nil))))


;;; paginator

(defclass cash-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg cash-paginator) start)
  (let ((table (table pg)))
    (apply #'cash (role table) :start start (filter table))))



;;; ----------------------------------------------------------------------
;;; Utilities
;;; ----------------------------------------------------------------------

(defun cash-debit-account-id (role account-id)
  (if (customer-p role)
      (account-id 'cash-account)
      account-id))

(defun cash-credit-account-id (role account-id)
  (if (customer-p role)
      account-id
      (account-id 'cash-account)))

(defun cash-page-title (role op-label)
  (conc "Μετρητά » " (if (customer-p role) "Έσοδα" "Έξοδα") " » " op-label))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun cash-top-actions (op)
  (let* ((role (first *registers*))
         (new-cash-label (conc "Νέο " (if (customer-p role) "Έσοδo" "Έξοδo"))))
    (top-actions-area
     (make-instance 'scrooge-menu
                    :spec (make-menu-spec
                           `(:catalogue ,(family-url 'cash :system :filter)
                             :create (,(family-url 'cash/create :filter) ,new-cash-label)))
                    :css-class "hmenu"
                    :disabled (list op))
     (searchbox (family-url-fn 'actions/cash/search)
                (family-url-fn 'cash :system)
                (family-params 'cash :filter)
                "ac-company"))))



;;; ------------------------------------------------------------
;;; Cash form
;;; ------------------------------------------------------------

(defclass cash-form (tx-form)
  ((role :accessor role :initarg :role)))

(defmethod display ((form cash-form) &key styles)
  (let* ((customer-p (customer-p (role form)))
         (disabled (eql (op form) :details))
         (record (record form))
         (ldfn (label-datum disabled record styles))
         (revenues/expenses-root-key (revenues/expenses-root (role form)))
         (receivable/payable-root-key (receivable/payable-root (role form)))
         (selected-key (or (getf record :account-id)
                           (getf record (if customer-p
                                            :credit-account-id
                                            :debit-account-id))
                           revenues/expenses-root-key)))
    (with-html
      (:div :id "ci-data-form" :class "data-form"
        (:div :class "grid_6 alpha"
          (:div :class "left-column"
            (:h3 "Στοιχεία Συναλλαγής")
            (display ldfn 'tx-date "Ημερομηνία":enabled-styles "datepicker"
                                               :default-value (today))
            (display ldfn 'description "Περιγραφή"
                     :common-styles "description")
            (display ldfn 'company "Εταιρία"
                     :enabled-styles "ac-company"
                     :href (company/details :company-id (getf record :company-id))
                     :common-styles "company")
            (display ldfn 'amount "Ποσό"
                     :common-styles "amount")
            (unless disabled
              (htm (:div :class "data-form-buttons"
                     (ok-button :body (if (eql (op form) :update)
                                          "Ανανέωση"
                                          "Δημιουργία"))
                     (cancel-button (cancel-url form)
                                    :body "Άκυρο"))))))
        (htm (:div :class "grid_5 omega"
               (:h3 (str (conc "Λογαριασμός " (if customer-p "πίστωσης" "χρέωσης"))))
               ;;
               (:div :class "hidden-when-immediate-tx-only"
                 (:h4 (str "Έναντι ανοιχτού λογαριασμού"))
                 (display (make-instance 'radio-account-tree
                                         :disabled disabled
                                         :root-key receivable/payable-root-key
                                         :debit-p customer-p
                                         :selected-key selected-key)))
               ;;
               (:div :class "company-dependent"
                 (:h4 (str (conc "Απ' ευθείας χρέωση σε λογαριασμό "
                                 (if customer-p "εσόδων" "εξόδων"))))
                 (display (make-instance 'radio-account-tree
                                         :disabled disabled
                                         :root-key revenues/expenses-root-key
                                         :debit-p (not customer-p)
                                         :selected-key selected-key)))))
        (clear)))))

(defmethod actions ((form cash-form) &key filter)
  (let* ((tx-id (key form))
         (role (role form))
         (hrefs (list :update (apply #'cash/update role :tx-id tx-id filter)
                      :delete (apply #'cash/delete role :tx-id tx-id filter))))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions form))))



;;; ----------------------------------------------------------------------
;;; SEARCH
;;; ----------------------------------------------------------------------

(defpage cash-page actions/cash/search
    (("actions/cash/" (role "(customer|supplier)") "/search") :request-type :get)
    ((search string)
     (since date)
     (until date))
  (with-db ()
    (let* ((filter (params->filter))
           (rows (rows (make-instance 'cash-tx-table :op :catalogue
                                                     :role role
                                                     :filter filter))))
      (if (single-item-list-p rows)
          (see-other (apply #'cash/details role
                            :tx-id (key (first rows))
                            filter))
          (see-other (apply #'cash role filter))))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defpage cash-page cash (("cash/" (role "(customer|supplier)")))
    ((tx-id  integer chk-tx-id)
     (start  integer)
     (search string)
     (since  date)
     (until  date))
  (check-cash-accounts)
  (with-view-page
    (let* ((filter (params->filter))
           (page-title (cash-page-title role "Κατάλογος"))
           (cash-tx-table (make-instance 'cash-tx-table
                                         :role role
                                         :op :catalogue
                                         :selected-key (val tx-id)
                                         :filter filter
                                         :start-index (val start))))
      ;; if tx-id exists and is not found among records, ignore search term
      (when (and (val tx-id)
                 (not (find (val tx-id) (rows cash-tx-table) :key #'key)))
        (let ((tx (get-dao 'tx (val tx-id))))
          (see-other (cash (cond ((eql (debit-account-id tx) (account-id 'cash-account))
                                  "customer")
                                 ((eql (credit-account-id tx) (account-id 'cash-account))
                                  "supplier")
                                 (t (error 'bad-request-error)))
                           :tx-id (val tx-id)))))
      ;; otherwise continue as usually
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'cash)
            (cash-top-actions :catalogue)
            (filters cash-tx-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" (str page-title))
                (actions cash-tx-table)
                (display cash-tx-table)))
            (footer)))))))

(defpage cash-page cash/details (("cash/" (role "(customer|supplier)") "/details"))
    ((tx-id  integer chk-tx-id t)
     (search string)
     (since  date)
     (until  date))
  (check-cash-accounts)
  (with-view-page
    (let* ((filter (params->filter))
           (cash-form (make-instance 'cash-form
                                     :role role
                                     :op :details
                                     :key (val tx-id)
                                     :cancel-url (apply #'cash role :tx-id (val tx-id) filter)))
           (page-title (cash-page-title role "Λεπτομέρειες")))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'cash)
            (cash-top-actions :details)
            (:div :class "grid_12"
              (:div :id "cash-window" :class "window"
                (:div :class "title" "Λεπτομέρειες")
                (actions cash-form :filter filter)
                (display cash-form)))
            (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage cash-page cash/create
    (("cash/" (role "(customer|supplier)") "/create"))
    ((tx-date     date)
     (description string)
     (company     string  chk-company-title)
     (amount      float   chk-amount)
     (account-id  integer chk-account-id)
     (search      string)
     (since       date)
     (until       date))
  (validate-parameters (chk-tx-constraints-fn role t) company)
  (check-cash-accounts)
  (with-view-page
    (let* ((filter (params->filter))
           (cash-form (make-instance 'cash-form
                                     :role role
                                     :op :create
                                     :cancel-url (apply #'cash role filter)))
           (page-title (cash-page-title role "Δημιουργία")))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'cash)
            (cash-top-actions :create)
            (:div :class "grid_12"
              (:div :id "cash-window" :class "window"
                (:div :class "title" (str page-title))
                (actions cash-form :filter filter)
                (notifications)
                (with-form (actions/cash/create role
                                                :search (val search)
                                                :since (val since)
                                                :until (val until))
                  (display cash-form :payload (params->payload)
                                     :styles (params->styles)))))
            (footer)))))))

(defpage cash-page actions/cash/create
    (("actions/cash/" (role "(customer|supplier)") "/create") :request-type :post)
    ((tx-date     date)
     (description string)
     (company     string  chk-company-title t)
     (amount      float   chk-amount t)
     (account-id  integer chk-account-id t)
     (search      string)
     (since       date)
     (until       date))
  (validate-parameters (chk-tx-constraints-fn role t) company)
  (check-cash-accounts)
  (with-controller-page (cash/create role)
    (let* ((company-id (company-id (val company)))
           (debit-account-id (cash-debit-account-id role (val account-id)))
           (credit-account-id (cash-credit-account-id role (val account-id)))
           (new-tx (make-instance 'tx
                                  :tx-date (val tx-date)
                                  :description (val description)
                                  :company-id company-id
                                  :amount (val amount)
                                  :credit-account-id credit-account-id
                                  :debit-account-id debit-account-id)))
      (insert-dao new-tx)
      (see-other (apply #'cash/details role :tx-id (tx-id new-tx)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage cash-page cash/update
    (("cash/" (role "(customer|supplier)") "/update"))
    ((search      string)
     (since       date)
     (until       date)
     (tx-id       integer chk-tx-id         t)
     (tx-date     date)
     (description string)
     (company     string  chk-company-title)
     (amount      float   chk-amount)
     (account-id  integer chk-account-id))
  (validate-parameters (chk-tx-constraints-fn role t) company)
  (check-cash-accounts)
  (with-view-page
    (let* ((filter (params->filter))
           (cash-form (make-instance 'cash-form
                                     :role role
                                     :op :update
                                     :key (val tx-id)
                                     :cancel-url (apply #'cash/details role
                                                        :tx-id (val tx-id)
                                                        filter)))
           (page-title (cash-page-title role "Επεξεργασία")))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'cash)
            (cash-top-actions :update)
            (:div :class "grid_12"
              (:div :id "cash-window" :class "window"
                (:div :class "title" (str page-title))
                (actions cash-form :filter filter)
                (notifications)
                (with-form (actions/cash/update role
                                                :tx-id (val tx-id)
                                                :search (val search)
                                                :since (val since)
                                                :until (val until))
                  (display cash-form :payload (params->payload)
                                     :styles (params->styles)))))
            (footer)))))))

(defpage cash-page actions/cash/update
    (("actions/cash/" (role "(customer|supplier)") "/update") :request-type :post)
    ((search      string)
     (since       date)
     (until       date)
     (tx-id       integer chk-tx-id         t)
     (tx-date     date)
     (description string)
     (company     string  chk-company-title)
     (amount      float   chk-amount)
     (account-id  integer chk-account-id))
  (validate-parameters (chk-tx-constraints-fn role t) company)
  (check-cash-accounts)
  (with-controller-page (cash/update role)
    (let ((company-id (company-id (val company)))
          (debit-account-id (cash-debit-account-id role (val account-id)))
          (credit-account-id (cash-credit-account-id role (val account-id))))
      (execute (:update 'tx :set
                        'tx-date (val tx-date)
                        'description (val description)
                        'company-id company-id
                        'amount (val amount)
                        'debit-account-id debit-account-id
                        'credit-account-id credit-account-id
                        :where (:= 'id (val tx-id))))
      (see-other (apply #'cash/details role :tx-id (val tx-id) (params->filter))))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage cash-page cash/delete
    (("cash/" (role "(customer|supplier)") "/delete"))
    ((tx-id  integer chk-tx-id/ref t)
     (search string)
     (since  date)
     (until  date))
  (check-cash-accounts)
  (with-view-page
    (let* ((filter (params->filter))
           (page-title (cash-page-title role "Διαγραφή"))
           (cash-tx-table (make-instance 'cash-tx-table
                                         :op :delete
                                         :role role
                                         :selected-key (val tx-id)
                                         :filter filter)))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'cash)
            (cash-top-actions :delete)
            (filters cash-tx-table)
            (:div :class "grid_12"
              (:div :id "cash-window" :class "window"
                (:div :class "title" (str page-title))
                (actions cash-tx-table)
                (with-form (actions/cash/delete role
                                                :tx-id (val tx-id)
                                                :search (val search)
                                                :since (val since)
                                                :until (val until))
                  (display cash-tx-table))))
            (footer)))))))

(defpage cash-page actions/cash/delete
    (("actions/cash/" (role "(customer|supplier)") "/delete") :request-type :post)
    ((tx-id  integer chk-tx-id/ref t)
     (search string)
     (since  date)
     (until  date))
  (check-cash-accounts)
  (with-controller-page (cash/delete)
    (delete-dao (get-dao 'tx (val tx-id)))
    (see-other (apply #'cash role (params->filter)))))
