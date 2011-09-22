(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass cheque-page (regex-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id))
   (payload-parameter-names
    :allocation :class
    :initform '(bank due-date company amount state account-id))
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
    '((due-date (:date-null
                 "Η ημερομηνία είναι κενή"
                 :parse-error
                 "Η ημερομηνία της επιταγής είναι άκυρη"))
      (bank (:bank-title-null
             "Το όνομα της τράπεζας είναι κενό."
             :bank-title-unknown
             "Δεν έχει καταχωρηθεί τράπεζα με αυτή την επωνυμία"))
      (company
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
        "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες"))))))



;;; --------------------------------------------------------------------------------
;;; Validation
;;; --------------------------------------------------------------------------------

(defun check-cheque-accounts ()
  (unless (and *cheque-receivable-acc-id*
               *cheque-payable-acc-id*
               *revenues-root-acc-id*
               *expenses-root-acc-id*)
    (see-other (cheque-accounts-error-page))))

(defpage dynamic-page cheque-accounts-error-page ("cheque/error")
    ()
  (with-document ()
    (:head
     (:title "Cheque accounts error")
     (error-headers))
    (:body
     (:div :id "header"
           (logo))
     (:div :id "body"
           (:div :id "content" :class "summary"
                 (:p "Δεν έχετε ορίσει στις ρυθμίσεις είτε ένα από τους λογαριασμούς επιταγών προς είσπραξη/πληρωμή, ή έναν από τους λογαριασμούς ρίζας εσόδων/εξόδων."))))))

(define-existence-predicate cheque-id-exists-p cheque id)

(defun chk-cheque-id (id)
  (if (cheque-id-exists-p id)
      nil
      :cheque-id-unknown))

(defun chk-date (date)
  (if (eql :null date)
      :date-null
      nil))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun cheque-page-title (kind)
  (cond ((string-equal kind "receivable") "Εισπρακτέες")
        ((string-equal kind "payable") "Πληρωτέες")
        (t (error "internal error in cheque-page-title"))))

(defun cheque-menu (kind id filter &optional disabled)
  (menu (crud+details-actions-spec (apply #'cheque kind         :id id filter)
                                   (apply #'cheque/create kind filter)
                                   (apply #'cheque/details kind :id id filter)
                                   (apply #'cheque/update kind  :id id filter)
                                   (apply #'cheque/delete kind  :id id filter))
        :id "cheque-actions"
        :css-class "hmenu actions"
        :disabled disabled))

(defun cheque-filters (kind)
  (let ((spec `((receivable ,(cheque "receivable") "Εισπρακτέες")
                (payable    ,(cheque "payable")    "Πληρωτέες"))))
    (with-html
      (:div :id "filters" :class "filters"
            (:p :class "title" "Είδος")
            (navbar spec
                    :id "cheque-kind-filter"
                    :css-class "vnavbar"
                    :active (intern (string-upcase kind)))))))

(defun cheque-state-filters (kind state search)
  (let ((spec `((nil      ,(cheque kind :search search) "Όλες")
                (pending  ,(cheque kind :search search :state "pending")  "Σε εκκρεμότητα")
                (paid     ,(cheque kind :search search :state "paid")     "Πληρωμένες")
                (bounced  ,(cheque kind :search search :state "bounced")  "Ακάλυπτες")
                (returned ,(cheque kind :search search :state "returned") "Επιστραμμένες"))))
    (with-html
      (:div :id "state-filters" :class "filters"
            (:p :class "title" "Κατάσταση")
            (navbar spec
                    :id "kind-filter"
                    :css-class "vnavbar"
                    :active (intern (string-upcase state)))))))



;;; ----------------------------------------------------------------------
;;; Database interface
;;; ----------------------------------------------------------------------

(defun cheque-record (id)
  (with-db ()
    (query (:select 'cheque.id (:as 'bank.title 'bank)
                    'due-date (:as 'company.title 'company)
                    'amount 'payable-p
                    :from 'cheque
                    :left-join 'bank
                    :on (:= 'bank.id 'cheque.bank-id)
                    :inner-join 'company
                    :on (:= 'company.id 'cheque.company-id)
                    :where (:= 'cheque.id id))
           :plist)))



;;; ----------------------------------------------------------------------
;;; Cheques table
;;; ----------------------------------------------------------------------

;;; table

(defclass cheque-table (scrooge-table)
  ((kind        :accessor kind :initarg :kind)
   (header-labels  :initform '("" "<br />Εταιρία" "<br />Τράπεζα"
                               "Ημερομηνία<br />πληρωμής" "<br />Ποσό"))
   (paginator      :initform (make-instance 'scrooge-paginator
                                            :id "cheque-paginator"
                                            :css-class "paginator")))
  (:default-initargs :item-class 'cheque-row))

(defmethod initialize-instance :after ((table cheque-table) &key)
  (let ((kind (kind table)))
    (setf (urlfn (paginator table))
          (lambda (&rest args)
            (apply #'cheque kind args)))))

(defmethod get-records ((table cheque-table))
  (let* ((search (getf (filter table) :search))
         (state (getf (filter table) :state))
         (payable-p (string= (kind table) "payable"))
         (base-query `(:select cheque.id (:as bank.title bank)
                               due-date (:as company.title company) amount payable-p
                               :from cheque
                               :inner-join bank
                               :on (:= bank.id cheque.bank-id)
                               :inner-join company
                               :on (:= company.id cheque.company-id)))
         (where-terms nil))
    (when search
      (push `(:or (:ilike company.title ,(ilike search))
                  (:ilike bank.title ,(ilike search)))
            where-terms))
    (when state
      (push `(:= cheque.state ,state)
            where-terms))
    (let* ((composite-query (append base-query
                                    `(:where (:and (:= cheque.payable-p ,payable-p)
                                                   ,@where-terms))))
           (final-query `(:order-by ,composite-query (:desc due-date))))
      (with-db ()
        (query (sql-compile final-query)
               :plists)))))


;;; rows

(defclass cheque-row (scrooge-row/plist)
  ())

(defmethod selector ((row cheque-row) enabled-p)
  (let* ((id (key row))
         (table (collection row))
         (pg (paginator table))
         (filter (filter table))
         (kind (kind table))
         (start (start-index table)))
    (html ()
      (:a :href (if enabled-p
                    (apply #'cheque kind
                           :start (page-start pg (index row) start)
                           filter)
                    (apply #'cheque kind :id id filter))
          (selector-img enabled-p)))))

(defmethod payload ((row cheque-row) enabled-p)
  (let ((record (record row)))
    (mapcar (lambda (name)
              (make-instance 'textbox
                             :name name
                             :value (getf record (make-keyword name))
                             :disabled (not enabled-p)))
            '(company bank due-date amount))))

(defmethod controls ((row cheque-row) enabled-p)
  (let* ((id (key row))
         (table (collection row))
         (filter (filter table))
         (cash-kind (kind table)))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (apply #'cash cash-kind :id id filter)))
        (list nil nil))))



;;; ------------------------------------------------------------
;;; Cheque form
;;; ------------------------------------------------------------

(defmethod display  ((form cheque-form) &key styles)
  (let* ((revenues-p (string-equal kind "receivable"))
         (disabled (eql (op form) :details))
         (record (record form))
         (lit (label-input-text disabled record styles))
         (tree (if (eql (op form) :create)
                   (make-account-radio-tree revenues-p)
                   nil)))
    (when (eql op :create)
      (push (root (make-instance 'account-radio-tree
                                 :root-key (if revenues-p
                                               *invoice-receivable-acc-id*
                                               *invoice-payable-acc-id*)
                                 :filter (list :debit-p revenues-p)))
            (children (root tree))))
    (with-html
      (:div :id "cheque-data-form" :class "data-form"
            (:div :class "grid_6"
                  (:div :class "data-form-first"
                        (display lit 'company "Εταιρία" "ac-company"))
                  (display lit 'bank "Τράπεζα" "ac-bank")
                  (display lit 'due-date "Ημερομηνία πληρωμής" "datepicker")
                  (display lit 'amount "Ποσό")
                  (label 'state "Κατάσταση")
                  (dropdown 'state
                            *cheque-states*
                            :selected (or (getf record :state) *default-cheque-state*)
                            :disabled disabled)
                  (:div :class "data-form-buttons"
                        (if disabled
                            (cancel-button (cancel-url form)
                                           :body "Επιστροφή στον Κατάλογο Επιταγών")
                            (progn
                              (ok-button :body (if (eql op :update) "Ανανέωση" "Δημιουργία"))
                              (cancel-button (cancel-url form)
                                             :body "Άκυρο")))))
            (:div :class "grid_6 data-form-first"
                  (label 'account (conc "Λογαριασμός " (if revenues-p "πίστωσης" "χρέωσης")))
                  ;; Display the tree. If needed, preselect the first account of the tree.
                  (display tree :key (or (getf record :account-id)
                                         (root-key tree))))))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage cheque-page cheque (("cheque/" (kind "(receivable|payable)")))
    ((search string)
     (state string)
     (start  integer)
     (id     integer chk-cheque-id))
  (with-view-page
    (check-cheque-accounts)
    (let* ((filter (params->filter))
           (page-title (conc "Επιταγές » " (cheque-page-title kind) " » Κατάλογος"))
           (cheque-table (make-instance 'cheque-table
                                        :id "cheque-table"
                                        :kind kind
                                        :op :read
                                        :filter filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cheque)
               (:div :class "window grid_10"
                     (:div :class "title" (str page-title))
                     (cheque-menu kind
                                  (val id)
                                  filter
                                  (if (val id)
                                      '(:read)
                                      '(:read :details :update :delete)))
                     (display cheque-table
                              :selected-id (val id)
                              :selected-data nil
                              :start (val start)))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (cheque kind) (val search))
                     (cheque-filters kind)
                     (cheque-state-filters kind (val state) (val search)))
               (footer)))))))

(defpage cheque-page cheque/details (("cheque/" (kind "(receivable|payable)") "/details"))
    ((search string)
     (state string)
     (id     integer chk-cheque-id t))
  (with-view-page
    (check-cheque-accounts)
    (let ((page-title (conc "Επιταγές » " (cheque-page-title kind) " » Λεπτομέρειες"))
          (filter (params->filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cheque)
               (:div :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cheque-menu kind
                                  (val id)
                                  filter
                                  '(:details :create)))
               (cheque-data-form kind :details
                                 :filter filter
                                 :id (val id)
                                 :data (cheque-record (val id)))))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/create (("cheque/" (kind  "(receivable|payable)") "/create"))
    ((search     string)
     (bank       string  chk-bank-title)
     (due-date   date    chk-date)
     (company    string  chk-company-title)
     (amount     float   chk-amount)
     (state     string  chk-cheque-state)
     (account-id integer chk-acc-id))
  (with-view-page
    (check-cheque-accounts)
    (let ((filter (params->filter))
          (page-title (conc "Επιταγές » " (cheque-page-title kind) " » Δημιουργία"))
          (cheque-form (make-instance 'cheque-form
                                      :kind kind
                                      :op :create
                                      :record nil
                                      :cancel-url (apply #'cheque kind filter))))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cheque)
               (:div :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cheque-menu kind
                                  nil
                                  filter
                                  '(:details :create :update :delete)))
               (with-form (actions/cheque/create kind :search (val search))
                 (display cheque-form :data (params->payload)
                                      :styles (params->styles)))
               (footer)))))))

(defpage cheque-page actions/cheque/create
    (("actions/cheque/" (kind "(receivable|payable)") "/create") :request-type :post)
    ((search     string)
     (bank       string  chk-bank-title)
     (company    string  chk-company-title t)
     (due-date   date    chk-date          t)
     (amount     float   chk-amount        t)
     (state     string  chk-cheque-state t)
     (account-id integer chk-acc-id        t))
  (with-controller-page (cheque/create)
    (check-cheque-accounts)
    (let* ((bank-id (bank-id (val bank)))
           (company-id (company-id (val company)))
           (new-cheque (make-instance 'cheque
                                      :bank-id bank-id
                                      :company-id company-id
                                      :due-date (val due-date)
                                      :amount (val amount)
                                      :state (val state)
                                      :payable-p (string= kind "payable"))))
      (with-db ()
        (with-transaction ()
          (insert-dao new-cheque)
          (let* ((debit-acc-id (if (string-equal kind "receivable")
                                   *cheque-receivable-acc-id*
                                   (val account-id)))
                 (credit-acc-id (if (string-equal kind "receivable")
                                    (val account-id)
                                    *cheque-payable-acc-id*))
                 (new-tx (make-instance 'tx
                                        :tx-date (today)
                                        :description (format nil "Auto-tx for new cheque ~A"
                                                             (id new-cheque))
                                        :company-id company-id
                                        :amount (val amount)
                                        :credit-acc-id credit-acc-id
                                        :debit-acc-id debit-acc-id
                                        :cheque-id (id new-cheque))))
            (insert-dao new-tx)))
        (see-other (cheque kind :id (id new-cheque) :state (val state)))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/update (("cheque/" (kind "(receivable|payable)") "/update"))
    ((search     string)
     (id         integer chk-cheque-id     t)
     (bank       string  chk-bank-title)
     (company    string  chk-company-title)
     (due-date   date    chk-date)
     (state     string  chk-cheque-state)
     (amount     float   chk-amount)
     (account-id integer chk-acc-id))
  (with-view-page
    (check-cheque-accounts)
    (let ((page-title (conc "Επιταγές » " (cheque-page-title kind) " » Επεξεργασία"))
          (filter (params->filter))
          (cheque-form (make-instance 'cheque-form
                                      :kind kind
                                      :op :create
                                      :record (get-record 'cheque (val id))
                                      :cancel-url (apply #'cheque kind :id (val id) filter))))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cheque)
               (:div :id "cheque-window" :class "window grid_12"
                     (:p :class "title" (str page-title))
                     (cheque-menu kind
                                  (val id)
                                  filter
                                  '(:create :update))
                     (notifications))
               (with-form (actions/cheque/update kind
                                                 :id (val id)
                                                 :search (val search))
                 (display cheque-form :payload (params->payload)
                                      :styles (params->styles)))
               (footer)))))))

(defpage cheque-page actions/cheque/update
    (("actions/cheque/" (kind "(receivable|payable)") "/update") :request-type :post)
    ((search     string)
     (id         integer chk-cheque-id     t)
     (bank       string  chk-bank-title)
     (company    string  chk-company-title t)
     (due-date   date    chk-date          t)
     (amount     float   chk-amount        t)
     (state     string  chk-cheque-state t)
     (account-id integer chk-acc-id        t))
  (with-controller-page (cheque/update)
    (check-cheque-accounts)
    (let* ((bank-id (bank-id (val bank)))
           (company-id (company-id (val company)))
           (debit-acc-id (if (string-equal kind "receivable")
                             *cheque-receivable-acc-id*
                             (val account-id)))
           (credit-acc-id (if (string-equal kind "receivable")
                              (val account-id)
                              *cheque-payable-acc-id*)))
      (with-db ()
        (with-transaction ()
          (let* ((cheque-dao (get-dao 'cheque (val id)))
                 (new-tx (if (string= state (state cheque-dao))
                             nil
                             (make-instance 'tx
                                            :tx-date (today)
                                            :description (conc "auto-tx for updated cheque "
                                                               (id cheque-dao))
                                            :company-id company-id
                                            :amount (val amount)
                                            :credit-acc-id credit-acc-id
                                            :debit-acc-id debit-acc-id
                                            :cheque-id (id cheque-dao)))))
            (setf (bank-id cheque-dao) bank-id
                  (company-id cheque-dao) company-id
                  (due-date cheque-dao) (val due-date)
                  (amount cheque-dao) (val amount)
                  (state cheque-dao) (val state))
            (update-dao cheque-dao)
            (insert-dao new-tx))))
      (see-other (cheque kind :id (val id) :state (val state))))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/delete (("cheque/" (kind "(receivable|payable)") "/delete"))
    ((search string)
     (state  string)
     (id     integer chk-cheque-id t))
  (with-view-page
    (check-cheque-accounts)
    (let* ((page-title (conc "Επιταγές » " (cheque-page-title kind) " » Διαγραφή"))
           (filter (params->filter))
           (cheque-table (make-instance 'cheque-table
                                        :op :delete
                                        :kind kind
                                        :id "cheque-table"
                                        :filter filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cheque)
               (:div :class "window"
                     (:div :class "window grid_10"
                           (:div :class "title" (str page-title))
                           (cheque-menu kind
                                        (val id)
                                        filter
                                        '(:read :create :delete))
                           (with-form (actions/cheque/delete kind
                                                             :id (val id)
                                                             :search (val search)
                                                             :state (val state))
                             (display cheque-table
                                      :selected-id (val id))))
                     (:div :id "sidebar" :class "sidebar grid_2"
                           (searchbox (cheque kind) (val search))
                           (cheque-filters kind)
                           (cheque-state-filters kind (val state) (val search))))
               (footer)))))))

(defpage cheque-page actions/cheque/delete
    (("actions/cheque/" (kind "(receivable|payable)") "/delete") :request-type :post)
    ((search string)
     (state string)
     (id     integer chk-cheque-id t))
  (with-controller-page ()
    (check-cheque-accounts)
    (delete-dao (get-dao 'cheque (val id)))
    (see-other (cheque kind :search (val search) :state (val state)))))
