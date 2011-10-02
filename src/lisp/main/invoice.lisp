(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass invoice-page (regex-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id))
   (payload-parameter-names
    :allocation :class
    :initform '(tx-date company description amount account-id))
   (filter-parameter-names
    :allocation :class
    :initform '(search since until))
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
  (unless (and *invoice-receivable-acc-id*
               *invoice-payable-acc-id*
               *revenues-root-acc-id*
               *expenses-root-acc-id*)
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
  ((header-labels  :initform '("" "Ημερομηνία" "Εταιρία" "Περιγραφή" "Ποσό" "" ""))
   (kind :accessor kind :initarg :kind)
   (paginator :initform (make-instance 'scrooge-paginator
                                       :css-class "paginator")))
  (:default-initargs :item-class 'invoice-tx-row))

(defmethod get-records ((table invoice-tx-table))
  (flet ((acc-kind (kind)
           (cond ((string-equal kind "receivable")
                  `(debit-acc-id ,*invoice-receivable-acc-id*))
                 ((string-equal kind "payable")
                  `(credit-acc-id ,*invoice-payable-acc-id*))
                 (t
                  (error "internal error in acc-kind")))))
    (let* ((search (getf (filter table) :search))
           (since (getf (filter table) :since))
           (until (getf (filter table) :until))
           (kind (kind table))
           (base-query `(:select tx.id tx-date
                                 (:as company.title company)
                                 (:as company.id 'company-id)
                                 description amount
                         :from tx
                         :left-join company
                         :on (:= tx.company-id company.id)))
           (where nil))
      (when search
        `(:or (:ilike description ,(ilike search))
              (:ilike company.title ,(ilike search))))
      (when (and since (not (eql since :null)))
        (push `(:<= ,since tx-date) where))
      (when (and until (not (eql until :null)))
        (push `(:<= tx-date ,until) where))
      (let ((sql `(:order-by (,@base-query :where
                                           (:and (:= ,@(acc-kind kind))
                                                 ,@where))
                             (:desc tx-date))))
        (with-db ()
          (query (sql-compile sql)
                 :plists))))))


;;; rows

(defclass invoice-tx-row (tx-row)
  ())

(defmethod selector ((row invoice-tx-row) selected-p)
  (let* ((id (key row))
         (table (collection row))
         (pg (paginator table))
         (filter (filter table))
         (kind (kind table))
         (start (start-index table)))
    (html ()
      (:a :href (if selected-p
                    (apply #'invoice kind :start (page-start pg (index row) start) filter)
                    (apply #'invoice kind :id id filter))
          (selector-img selected-p)))))

(defmethod payload ((row invoice-tx-row) enabled-p)
  (let ((record (record row)))
    (insert-list 1
                 (html ()
                   (:a :href (company/details :id (getf record :company-id))
                       (str (getf record :company))))
                 (mapcar (lambda (name)
                           (make-instance 'textbox
                                          :name name
                                          :value (getf record (make-keyword name))
                                          :disabled (not enabled-p)))
                         '(tx-date description amount)))))

(defmethod controls ((row invoice-tx-row) controls-p)
  (let* ((id (key row))
         (table (collection row))
         (filter (filter table))
         (kind (kind table)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button :href (apply #'invoice kind :id id filter)))
        (list nil nil))))


;;; paginator

(defclass invoice-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg invoice-paginator) start)
  (let ((table (table pg)))
   (apply #'invoice (kind table) :start start (filter table))))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun receivable-p (kind)
  (string-equal kind "receivable"))

(defun invoice-page-title (kind)
  (cond ((string-equal kind "receivable") "Προς είσπραξη")
        ((string-equal kind "payable") "Προς πληρωμή")
        (t (error "Internal error in invoice-page-title"))))

(defun invoice-actions (op kind id filter)
  (actions-menu (crud+details-actions-spec (apply #'invoice/create kind filter)
                                           (apply #'invoice/details kind :id id filter)
                                           (apply #'invoice/update kind :id id filter)
                                           (apply #'invoice/delete kind :id id filter))
                (crud+details-actions-enabled/disabled op id)))

(defun invoice-filters (kind filter)
  (with-html
    (:div :class "filters"
          (filters-navbar `((receivable ,(apply #'invoice "receivable" filter) "Προς είσπραξη")
                            (payable ,(apply #'invoice "payable" filter) "Προς πληρωμή"))
                          kind)
          (datebox (lambda (&rest args)
                     (apply #'invoice kind args))
                   filter))))

(defun invoice-subnavbar (op kind filter)
  (with-html
    (:div :class "section-subnavbar grid_12"
          (if (member op '(:catalogue :delete))
              (invoice-filters kind filter)
              (htm (:div :class "options"
                         (:ul (:li (:a :href (apply #'invoice kind filter)
                                       "Κατάλογος"))))))
          (searchbox #'(lambda (&rest args)
                         (apply #'invoice kind args))
                     filter
                     "ac-company"))))


;;; ----------------------------------------------------------------------
;;; Invoice form
;;; ----------------------------------------------------------------------

(defclass invoice-form (crud-form/plist)
  ((kind :accessor kind :initarg :kind)))

(defmethod display ((form invoice-form) &key styles)
  (let* ((receivable-p (receivable-p (kind form)))
         (disabled (eql (op form) :details))
         (record (record form))
         (lit (label-input-text disabled record styles))
         (tree (make-instance 'rev/exp-account-tree
                              :disabled disabled
                              :root-key (if receivable-p
                                            *revenues-root-acc-id*
                                            *expenses-root-acc-id*)
                              :filter (list :debit-p (not receivable-p)))))
    (with-html
      (:div :id "invoice-data-form" :class "data-form"
            (:div :class "grid_6 alpha"
                  (display lit 'tx-date "Ημερομηνία" "datepicker")
                  (display lit 'description "Περιγραφή")
                  (display lit 'company "Εταιρία" "ac-company")
                  (display lit 'amount "Ποσό")
                  (unless disabled
                    (htm (:div :class "data-form-buttons"
                               (ok-button :body (if (eql (op form) :update)
                                                    "Ανανέωση"
                                                    "Δημιουργία"))
                               (cancel-button (cancel-url form)
                                              :body "Άκυρο")))))
            (:div :class "grid_6 omega"
                  (label 'account-id (conc "Λογαριασμός "
                                           (if receivable-p "εσόδων" "εξόδων")))
                  ;; Display the tree. If needed, preselect the first account of the tree.
                  (display tree :key (or (getf record (if receivable-p
                                                          :credit-acc-id
                                                          :debit-acc-id))
                                         (root-key tree))))))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defpage invoice-page invoice (("invoice/" (kind "(receivable|payable)")))
    ((search    string)
     (start     integer)
     (since     date    )
     (until     date    )
     (id        integer chk-tx-id))
  (with-view-page
    (check-invoice-accounts)
    (break "since ~A" since)
    (let* ((op :catalogue)
           (filter (params->filter))
           (page-title (conc (invoice-page-title kind) " » Κατάλογος"))
           (invoice-tx-table (make-instance 'invoice-tx-table
                                            :id "invoice-tx-table"
                                            :kind kind
                                            :op :catalogue
                                            :filter filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'invoice)
               (invoice-subnavbar op kind filter)
               (:div :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (invoice-actions op kind (val id) filter)
                     (display invoice-tx-table
                              :key (val id)
                              :payload nil
                              :start (val start)))
               (footer)))))))

(defpage invoice-page invoice/details (("invoice/" (kind "(receivable|payable)") "/details"))
    ((search  string)
     (cstate  string)
     (since   date)
     (until   date)
     (id      integer chk-tx-id t))
  (with-view-page
    (let* ((op :details)
           (filter (params->filter))
           (invoice-form (make-instance 'invoice-form
                                        :kind kind
                                        :op op
                                        :record (get-record 'tx (val id))
                                        :cancel-url (apply #'tx :id (val id) filter)))
           (page-title (conc (invoice-page-title kind) " » Λεπτομέρειες")))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'invoice)
               (invoice-subnavbar op kind filter)
               (:div :id "invoice-window" :class "window grid_12"
                     (:p :class "title" "Λεπτομέρειες")
                     (invoice-actions op kind (val id) filter)
                     (display invoice-form))
               (footer)))))))


;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage invoice-page invoice/create
    (("invoice/" (kind  "(receivable|payable)") "/create"))
    ((search      string)
     (since       date)
     (until       date)
     (tx-date     date)
     (company     string  chk-company-title)
     (description string)
     (amount      float   chk-amount)
     (account-id  integer chk-acc-id))
  (with-view-page
    (check-invoice-accounts)
    (let* ((op :create)
           (filter (params->filter))
           (invoice-form (make-instance 'invoice-form
                                        :kind kind
                                        :op op
                                        :record nil
                                        :cancel-url (apply #'invoice kind filter)))
           (page-title (conc (invoice-page-title kind) " » Δημιουργία")))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'invoice)
               (invoice-subnavbar op kind filter)
               (:div :id "invoice-window" :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (invoice-actions op kind nil filter)
                     (notifications)
                     (with-form (actions/invoice/create kind
                                                        :search (val search)
                                                        :since (val since)
                                                        :until (val until))
                       (display invoice-form :payload (params->payload)
                                             :styles (params->styles))))
               (footer)))))))

(defpage invoice-page actions/invoice/create
    (("actions/invoice/" (kind "(receivable|payable)") "/create")
     :request-type :post)
    ((search      string)
     (since       date)
     (until       date)
     (tx-date     date)
     (company     string  chk-company-title t)
     (description string)
     (amount      float   chk-amount t)
     (account-id  integer chk-acc-id t))
  (with-controller-page (invoice/create kind)
    (check-invoice-accounts)
    (let* ((company-id (company-id (val company)))
           (debit-acc-id (if (string-equal kind "receivable")
                             *invoice-receivable-acc-id*
                             (val account-id)))
           (credit-acc-id (if (string-equal kind "receivable")
                              (val account-id)
                              *invoice-payable-acc-id*))
           (new-tx (make-instance 'tx
                                  :tx-date (val tx-date)
                                  :description (val description)
                                  :company-id company-id
                                  :amount (val amount)
                                  :credit-acc-id credit-acc-id
                                  :debit-acc-id debit-acc-id
                                  :auto t)))
      (insert-dao new-tx)
      (see-other (apply #'invoice/details kind :id (tx-id new-tx)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage invoice-page invoice/update
    (("invoice/" (kind "(receivable|payable)") "/update"))
    ((search      string)
     (since       date)
     (until       date)
     (id          integer chk-tx-id t)
     (tx-date     date)
     (company     string  chk-company-title)
     (description string)
     (amount      float   chk-amount)
     (account-id  integer chk-acc-id))
  (with-view-page
    (check-invoice-accounts)
    (let* ((op :update)
           (filter (params->filter))
           (invoice-form (make-instance 'invoice-form
                                        :kind kind
                                        :op op
                                        :record (get-record 'tx (val id))
                                        :cancel-url (apply #'invoice kind :id (val id) filter)))
           (page-title (conc (invoice-page-title kind) " » Επεξεργασία")))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'invoice)
               (invoice-subnavbar op kind filter)
               (:div :id "invoice-window" :class "window grid_12"
                     (:p :class "title" "Επεξεργασία")
                     (invoice-actions op kind (val id) filter)
                     (notifications)
                     (with-form (actions/invoice/update kind
                                                        :id (val id)
                                                        :search (val search)
                                                        :since (val since)
                                                        :until (val until))
                       (display invoice-form :payload (params->payload)
                                             :styles (params->styles))))
               (footer)))))))

(defpage invoice-page actions/invoice/update
    (("actions/invoice/" (kind "(receivable|payable)") "/update") :request-type :post)
    ((search      string)
     (since       date)
     (until       date)
     (id          integer chk-tx-id         t)
     (tx-date     date)
     (description string)
     (company     string  chk-company-title)
     (amount      float   chk-amount)
     (account-id  integer chk-acc-id))
  (with-controller-page (invoice/update kind)
    (check-invoice-accounts)
    (let ((company-id (company-id (val company)))
          (debit-acc-id (if (string-equal kind "receivable")
                            *invoice-receivable-acc-id*
                            (val account-id)))
          (credit-acc-id (if (string-equal kind "receivable")
                             (val account-id)
                             *invoice-payable-acc-id*)))
      (execute (:update 'tx :set
                        'tx-date (val tx-date)
                        'description (val description)
                        'company-id company-id
                        'amount (val amount)
                        'debit-acc-id debit-acc-id
                        'credit-acc-id credit-acc-id
                        :where (:= 'id (val id))))
      (see-other (apply #'invoice/details kind :id (val id)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage invoice-page invoice/delete
    (("invoice/" (kind "(receivable|payable)") "/delete"))
    ((id     integer chk-tx-id t)
     (search string)
     (since  date)
     (until  date))
  (with-view-page
    (check-invoice-accounts)
    (let* ((op :delete)
           (filter (params->filter))
           (page-title (conc (invoice-page-title kind) " » Διαγραφή"))
           (invoice-tx-table (make-instance 'invoice-tx-table
                                            :op op
                                            :kind kind
                                            :filter filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'invoice)
               (invoice-subnavbar op kind filter)
               (:div :id "invoice-window" :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (invoice-actions op kind (val id) filter)
                     (with-form (actions/invoice/delete kind
                                                        :id (val id)
                                                        :search (val search)
                                                        :since (val since)
                                                        :until (val until))
                       (display invoice-tx-table
                                :key (val id))))
               (footer)))))))

(defpage invoice-page actions/invoice/delete
    (("actions/invoice/" (kind "(receivable|payable)") "/delete") :request-type :post)
    ((search string)
     (since  date)
     (until  date)
     (id     integer chk-tx-id t))
  (with-controller-page (invoice/delete kind)
    (check-invoice-accounts)
    (delete-dao (get-dao 'tx (val id)))
    (see-other (apply #'invoice kind
                      (params->filter)))))
