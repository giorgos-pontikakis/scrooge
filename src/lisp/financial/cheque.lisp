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
      (company (:company-title-null
                "Το όνομα της εταιρίας είναι κενό"
                :company-title-unknown
                "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"))
      (amount (:non-positive-amount
               "Το ποσό της επιταγής πρέπει να είναι θετικός αριθμός"
               :parse-error
               "Το ποσό της επιταγής περιέχει άκυρους χαρακτήρες"))))))



;;; --------------------------------------------------------------------------------
;;; Validation
;;; --------------------------------------------------------------------------------

(defun check-cheque-accounts ()
  (unless (and *cheque-receivable-account*
               *cheque-payable-account*
               *revenues-root-account*
               *expenses-root-account*)
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

(defun cheque-kind-label (cheque-kind)
  (cond ((string-equal cheque-kind "receivable") "Εισπρακτέες")
        ((string-equal cheque-kind "payable") "Πληρωτέες")
        (t (error "internal error in cheque-kind-label"))))

(defun cheque-menu (cheque-kind id filter &optional disabled)
  (menu (crud+details-actions-spec (apply #'cheque cheque-kind         :id id filter)
                                   (apply #'cheque/create cheque-kind filter)
                                   (apply #'cheque/details cheque-kind :id id filter)
                                   (apply #'cheque/update cheque-kind  :id id filter)
                                   (apply #'cheque/delete cheque-kind  :id id filter))
        :id "cheque-actions"
        :css-class "hmenu actions"
        :disabled disabled))

(defun cheque-notifications ()
  (notifications
   ))

(defun cheque-filters (cheque-kind)
  (let ((spec `((receivable ,(cheque "receivable") "Εισπρακτέες")
                (payable    ,(cheque "payable")    "Πληρωτέες"))))
    (with-html
      (:div :id "filters" :class "filters"
            (:p :class "title" "Είδος")
            (navbar spec
                    :id "cheque-kind-filter"
                    :css-class "vnavbar"
                    :active (intern (string-upcase cheque-kind)))))))

(defun cheque-state-filters (cheque-kind state search)
  (let ((spec `((nil      ,(cheque cheque-kind :search search) "Όλες")
                (pending  ,(cheque cheque-kind :search search :state "pending")  "Σε εκκρεμότητα")
                (paid     ,(cheque cheque-kind :search search :state "paid")     "Πληρωμένες")
                (bounced  ,(cheque cheque-kind :search search :state "bounced")  "Ακάλυπτες")
                (returned ,(cheque cheque-kind :search search :state "returned") "Επιστραμμένες"))))
    (with-html
      (:div :id "state-filters" :class "filters"
            (:p :class "title" "Κατάσταση")
            (navbar spec
                    :id "cheque-kind-filter"
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
  ((subpage        :accessor subpage :initarg :subpage)
   (header-labels  :initform '("" "<br />Εταιρία" "<br />Τράπεζα"
                               "Ημερομηνία<br />πληρωμής" "<br />Ποσό"))
   (paginator      :initform (make-instance 'scrooge-paginator
                                            :id "cheque-paginator"
                                            :css-class "paginator")))
  (:default-initargs :item-class 'cheque-row))

(defmethod initialize-instance :after ((table cheque-table) &key)
  (let ((cheque-kind (subpage table)))
    (setf (urlfn (paginator table))
          (lambda (&rest args)
            (apply #'cheque cheque-kind args)))))

(defmethod get-records ((table cheque-table))
  (let* ((search (getf (filter table) :search))
         (state (getf (filter table) :state))
         (payable-p (string= (subpage table) "payable"))
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
         (cheque-kind (subpage table))
         (start (start-index table)))
    (html ()
      (:a :href (if enabled-p
                    (apply #'cheque cheque-kind
                           :start (page-start pg (index row) start)
                           filter)
                    (apply #'cheque cheque-kind :id id filter))
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
         (cash-kind (subpage table)))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (apply #'cash cash-kind :id id filter)))
        (list nil nil))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defun cheque-data-form (cheque-kind op &key filter id data styles)
  (let* ((revenues-p (string-equal cheque-kind "receivable"))
         (disabled (eql op :details))
         (tree (if (eql op :create)
                   (make-account-radio-tree revenues-p)
                   nil)))
    (when (eql op :create)
      (push (root (make-instance 'account-radio-tree
                                 :root-key (if revenues-p
                                               *invoice-receivable-account*
                                               *invoice-payable-account*)
                                 :filter (list :debit-p revenues-p)))
            (children (root tree))))
    (flet ((label-input-text (name label &optional extra-styles)
             (with-html
               (label name label)
               (input-text name
                           :value (getf data (make-keyword name))
                           :disabled disabled
                           :css-class (conc (getf styles (make-keyword name))
                                            " " extra-styles)))))
      (with-html
        (:div :id "cheque-data-form" :class "data-form"
              (:div :class "grid_6"
                    (:div :class "data-form-first"
                          (label-input-text 'company "Εταιρία" "ac-company"))
                    (label-input-text 'bank "Τράπεζα" "ac-bank")
                    (label-input-text 'due-date "Ημερομηνία πληρωμής" "datepicker")
                    (label-input-text 'amount "Ποσό")
                    (label 'state "Κατάσταση")
                    (dropdown 'state
                              *cheque-states*
                              :selected (or (getf data :state) *default-cheque-state*)
                              :disabled disabled)
                    (:div :class "data-form-buttons"
                          (if disabled
                              (cancel-button (apply #'cheque cheque-kind :id id filter)
                                             :body "Επιστροφή στον Κατάλογο Επιταγών")
                              (progn
                                (ok-button :body (if (eql op :update) "Ανανέωση" "Δημιουργία"))
                                (cancel-button (apply #'cheque cheque-kind :id id filter)
                                               :body "Άκυρο")))))
              (:div :class "grid_6 data-form-first"
                    (label 'account (conc "Λογαριασμός " (if revenues-p "πίστωσης" "χρέωσης")))
                    ;; Display the tree. If needed, preselect the first account of the tree.
                    (display tree :selected-id (or (getf data :account-id)
                                                   (key (first (children (root tree))))))))))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage cheque-page cheque (("cheque/" (cheque-kind "(receivable|payable)")))
    ((search string)
     (state string)
     (start  integer)
     (id     integer chk-cheque-id))
  (with-view-page
    (check-cheque-accounts)
    (let* ((filter (params->filter))
           (page-title (conc "Επιταγές » " (cheque-kind-label cheque-kind) " » Κατάλογος"))
           (cheque-table (make-instance 'cheque-table
                                        :id "cheque-table"
                                        :subpage cheque-kind
                                        :op :read
                                        :filter filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (financial-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'financial)
               (financial-navbar 'cheque)
               (:div :class "window grid_10"
                     (:div :class "title" (str page-title))
                     (cheque-menu cheque-kind
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
                     (searchbox (cheque cheque-kind) (val search))
                     (cheque-filters cheque-kind)
                     (cheque-state-filters cheque-kind (val state) (val search)))
               (footer)))))))

(defpage cheque-page cheque/details (("cheque/" (cheque-kind "(receivable|payable)") "/details"))
    ((search string)
     (state string)
     (id     integer chk-cheque-id t))
  (with-view-page
    (check-cheque-accounts)
    (let ((page-title (conc "Επιταγές » " (cheque-kind-label cheque-kind) " » Λεπτομέρειες"))
          (filter (params->filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (financial-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'financial)
               (financial-navbar 'cheque)
               (:div :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cheque-menu cheque-kind
                                  (val id)
                                  filter
                                  '(:details :create)))
               (cheque-data-form cheque-kind :details
                                 :filter filter
                                 :id (val id)
                                 :data (cheque-record (val id)))))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/create (("cheque/" (cheque-kind  "(receivable|payable)") "/create"))
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
          (page-title (conc "Επιταγές » " (cheque-kind-label cheque-kind) " » Δημιουργία")))
      (with-document ()
        (:head
         (:title (str page-title))
         (financial-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'financial)
               (financial-navbar 'cheque)
               (:div :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cheque-menu cheque-kind
                                  nil
                                  filter
                                  '(:details :create :update :delete)))
               (with-form (actions/cheque/create cheque-kind :search (val search))
                 (cheque-data-form cheque-kind :create
                                   :filter filter
                                   :data (params->payload bank
                                                          company
                                                          due-date
                                                          amount
                                                          state
                                                          account-id)
                                   :styles (params->styles bank
                                                           company
                                                           due-date
                                                           amount
                                                           state
                                                           account-id)))
               (footer)))))))

(defpage cheque-page actions/cheque/create
    (("actions/cheque/" (cheque-kind "(receivable|payable)") "/create") :request-type :post)
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
                                      :payable-p (string= cheque-kind "payable"))))
      (with-db ()
        (with-transaction ()
          (insert-dao new-cheque)
          (let* ((debit-acc-id (if (string-equal cheque-kind "receivable")
                                   *cheque-receivable-account*
                                   (val account-id)))
                 (credit-acc-id (if (string-equal cheque-kind "receivable")
                                    (val account-id)
                                    *cheque-payable-account*))
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
        (see-other (cheque cheque-kind :id (id new-cheque) :state (val state)))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/update (("cheque/" (cheque-kind "(receivable|payable)") "/update"))
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
    (let ((page-title (conc "Επιταγές » " (cheque-kind-label cheque-kind) " » Επεξεργασία"))
          (filter (params->filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (financial-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'financial)
               (financial-navbar 'cheque)
               (:div :id "cheque-window" :class "window grid_12"
                     (:p :class "title" (str page-title))
                     (cheque-menu cheque-kind
                                  (val id)
                                  filter
                                  '(:create :update))
                     (cheque-notifications))
               (with-form (actions/cheque/update cheque-kind
                                                 :id (val id)
                                                 :search (val search))
                 (cheque-data-form cheque-kind :update
                                   :id (val id)
                                   :filter filter
                                   :data (plist-union (params->payload bank
                                                                       company
                                                                       due-date
                                                                       amount
                                                                       state
                                                                       account-id)
                                                      (cheque-record (val id)))
                                   :styles (params->styles bank
                                                           company
                                                           due-date
                                                           amount
                                                           state
                                                           account-id)))
               (footer)))))))

(defpage cheque-page actions/cheque/update
    (("actions/cheque/" (cheque-kind "(receivable|payable)") "/update") :request-type :post)
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
           (debit-acc-id (if (string-equal cheque-kind "receivable")
                             *cheque-receivable-account*
                             (val account-id)))
           (credit-acc-id (if (string-equal cheque-kind "receivable")
                              (val account-id)
                              *cheque-payable-account*)))
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
      (see-other (cheque cheque-kind :id (val id) :state (val state))))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/delete (("cheque/" (cheque-kind "(receivable|payable)") "/delete"))
    ((search string)
     (state string)
     (id     integer chk-cheque-id t))
  (with-view-page
    (check-cheque-accounts)
    (let* ((page-title (conc "Επιταγές » " (cheque-kind-label cheque-kind) " » Διαγραφή"))
           (filter (params->filter))
           (cheque-table (make-instance 'cheque-table
                                        :op :delete
                                        :subpage cheque-kind
                                        :id "cheque-table"
                                        :filter filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (financial-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'financial)
               (financial-navbar 'cheque)
               (:div :class "window"
                     (:div :class "window grid_10"
                           (:div :class "title" (str page-title))
                           (cheque-menu cheque-kind
                                        (val id)
                                        filter
                                        '(:read :create :delete))
                           (with-form (actions/cheque/delete cheque-kind
                                                             :id (val id)
                                                             :search (val search)
                                                             :state (val state))
                             (display cheque-table
                                      :selected-id (val id))))
                     (:div :id "sidebar" :class "sidebar grid_2"
                           (searchbox (cheque cheque-kind) (val search))
                           (cheque-filters cheque-kind)
                           (cheque-state-filters cheque-kind (val state) (val search))))
               (footer)))))))

(defpage cheque-page actions/cheque/delete
    (("actions/cheque/" (cheque-kind "(receivable|payable)") "/delete") :request-type :post)
    ((search string)
     (state string)
     (id     integer chk-cheque-id t))
  (with-controller-page ()
    (check-cheque-accounts)
    (delete-dao (get-dao 'cheque (val id)))
    (see-other (cheque cheque-kind :search (val search) :state (val state)))))
