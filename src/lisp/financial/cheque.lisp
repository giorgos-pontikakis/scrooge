(in-package :scrooge)



;;; --------------------------------------------------------------------------------
;;; Validation
;;; --------------------------------------------------------------------------------

(defun check-cheque-accounts ()
  (unless (and *cheque-receivable-account*
               *cheque-payable-account*
               *revenues-root-account*
               *expenses-root-account*)
    (see-other (cheque-accounts-error-page))))

(define-dynamic-page cheque-accounts-error-page ("financial/cash/error")
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



;;; --------------------------------------------------------------------------------
;;; Actions
;;; --------------------------------------------------------------------------------

(define-regex-page actions/financial/cheque/create
    (("actions/financial/cheque/" (cheque-kind "receivable|payable") "/create") :request-type :post)
    ((search     string)
     (bank       string  chk-bank-title)
     (company    string  chk-company-title t)
     (due-date   date    chk-date          t)
     (amount     float   chk-amount        t)
     (status     string  chk-cheque-status t)
     (account-id integer chk-acc-id        t))
  (with-auth ("configuration")
    (no-cache)
    (check-cheque-accounts)
    (if (every #'validp (parameters *page*))
        (let* ((bank-id (bank-id (val bank)))
               (company-id (company-id (val company)))
               (new-cheque (make-instance 'cheque
                                          :bank-id bank-id
                                          :company-id company-id
                                          :due-date (val due-date)
                                          :amount (val amount)
                                          :status (val status)
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
            (see-other (cheque cheque-kind :id (id new-cheque) :status (val status)))))
        (see-other (cheque/create cheque-kind
                                  :search (raw search)
                                  :bank (raw bank)
                                  :company (raw company)
                                  :due-date (raw due-date)
                                  :amount (raw amount)
                                  :status (raw status)
                                  :account-id (raw account-id))))))

(define-regex-page actions/financial/cheque/update
    (("actions/financial/cheque/" (cheque-kind "receivable|payable") "/update") :request-type :post)
    ((search     string)
     (id         integer chk-cheque-id     t)
     (bank       string  chk-bank-title)
     (company    string  chk-company-title t)
     (due-date   date    chk-date          t)
     (amount     float   chk-amount        t)
     (status     string  chk-cheque-status t)
     (account-id integer chk-acc-id        t))
  (with-auth ("configuration")
    (no-cache)
    (check-cheque-accounts)
    (if (every #'validp (parameters *page*))
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
                     (new-tx (if (string= status (status cheque-dao))
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
                      (status cheque-dao) (val status))
                (update-dao cheque-dao)
                (insert-dao new-tx))))
          (see-other (cheque cheque-kind :id (val id) :status (val status))))
        (see-other (cheque/update cheque-kind
                                  :search (raw search)
                                  :id (raw id)
                                  :bank (raw bank)
                                  :company (raw company)
                                  :due-date (raw due-date)
                                  :amount (raw amount)
                                  :status (raw status)
                                  :account-id (raw account-id))))))

(define-regex-page actions/financial/cheque/delete
    (("actions/financial/cheque/" (cheque-kind "receivable|payable") "/delete") :request-type :post)
    ((search string)
     (status string)
     (id     integer chk-cheque-id t))
  (with-auth ("configuration")
    (no-cache)
    (check-cheque-accounts)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'cheque (val id)))
          (see-other (cheque cheque-kind :search (val search) :status (val status))))
        (see-other (notfound)))))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun cheque-kind-label (cheque-kind)
  (cond ((string-equal cheque-kind "receivable") "Εισπρακτέες")
        ((string-equal cheque-kind "payable") "Πληρωτέες")
        (t (error "internal error in cheque-kind-label"))))

(defun cheque-menu (cheque-kind id filter &optional disabled-items)
  (display
   (make-instance 'actions-menu
                  :id "cheque-actions"
                  :style "hnavbar actions"
                  :spec (crud+details-actions-spec
                         (apply #'cheque cheque-kind         :id id filter)
                         (apply #'cheque/create cheque-kind filter)
                         (apply #'cheque/details cheque-kind :id id filter)
                         (apply #'cheque/update cheque-kind  :id id filter)
                         (apply #'cheque/delete cheque-kind  :id id filter)))
   :disabled-items disabled-items))

(defun cheque-notifications ()
  (notifications
   '((due-date (:date-null "Η ημερομηνία είναι κενή"
                :parse-error "Η ημερομηνία της επιταγής είναι άκυρη"))
     (bank (:bank-title-null "Το όνομα της τράπεζας είναι κενό."
            :bank-title-unknown "Δεν έχει καταχωρηθεί τράπεζα με αυτή την επωνυμία"))
     (company (:company-title-null "Το όνομα της εταιρίας είναι κενό"
               :company-title-unknown "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"))
     (amount (:non-positive-amount  "Το ποσό της επιταγής πρέπει να είναι θετικός αριθμός"
              :parse-error  "Το ποσό της επιταγής περιέχει άκυρους χαρακτήρες")))))

(defun cheque-filters (cheque-kind)
  (let ((spec `((receivable ,(cheque "receivable") "Εισπρακτέες")
                (payable    ,(cheque "payable")    "Πληρωτέες"))))
    (with-html
      (:div :id "filters" :class "filters"
            (:p :class "title" "Είδος")
            (display (make-instance 'vertical-navbar
                                    :id "cheque-kind-filter"
                                    :style "vnavbar"
                                    :spec spec)
                     :active-page-name (intern (string-upcase cheque-kind)))))))

(defun cheque-status-filters (cheque-kind status search)
  (let ((spec `((nil      ,(cheque cheque-kind :search search) "Όλες")
                (pending  ,(cheque cheque-kind :search search :status "pending")  "Σε εκκρεμότητα")
                (paid     ,(cheque cheque-kind :search search :status "paid")     "Πληρωμένες")
                (bounced  ,(cheque cheque-kind :search search :status "bounced")  "Ακάλυπτες")
                (returned ,(cheque cheque-kind :search search :status "returned") "Επιστραμμένες"))))
    (with-html
      (:div :id "status-filters" :class "filters"
            (:p :class "title" "Κατάσταση")
            (display (make-instance 'vertical-navbar
                                    :id "cheque-kind-filter"
                                    :style "vnavbar"
                                    :spec spec)
                     :active-page-name (intern (string-upcase status)))))))



;;; ----------------------------------------------------------------------
;;; Database interface
;;; ----------------------------------------------------------------------

(defun get-cheque-plist (id)
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

(defclass cheque-table (crud-table)
  ((subpage        :accessor subpage :initarg :subpage)
   (item-key-field :initform :id)
   (header-labels  :initform '("" "<br />Εταιρία" "<br />Τράπεζα"
                               "Ημερομηνία<br />πληρωμής" "<br />Ποσό"))
   (paginator      :initform (make-instance 'default-paginator
                                            :id "cheque-paginator"
                                            :style "paginator")))
  (:default-initargs :item-class 'cheque-row))

(defmethod initialize-instance :after ((table cheque-table) &key)
  (let ((cheque-kind (subpage table)))
    (setf (urlfn (paginator table))
          (lambda (&rest args)
            (apply #'cheque cheque-kind args)))))

(defmethod read-records ((table cheque-table))
  (let* ((search (getf (filter table) :search))
         (status (getf (filter table) :status))
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
    (when status
      (push `(:= cheque.status ,status)
            where-terms))
    (let* ((composite-query (append base-query
                                    `(:where (:and (:= cheque.payable-p ,payable-p)
                                                   ,@where-terms))))
           (final-query `(:order-by ,composite-query (:desc due-date))))
      (with-db ()
        (query (sql-compile final-query)
               :plists)))))


;;; rows

(defclass cheque-row (crud-row)
  ())

(defmethod cells ((row cheque-row) &key start)
  (let* ((id (key row))
         (record (record row))
         (pg (paginator (collection row)))
         (filter (filter (collection row)))
         (cheque-kind (if (getf record :payable-p) "payable" "receivable")))
    (list :selector
          (make-instance 'selector-cell
                         :states (list :on (apply #'cheque cheque-kind
                                                  :start (page-start pg (index row) start)
                                                  filter)
                                       :off (apply #'cheque cheque-kind :id id filter)))
          :payload
          (mapcar (lambda (name)
                    (make-instance 'textbox-cell
                                   :name name
                                   :value (getf record (make-keyword name))))
                  '(company bank due-date amount))
          :controls
          (list
           (make-instance 'ok-cell)
           (make-instance 'cancel-cell
                          :href (apply #'cheque cheque-kind :id id filter))))))



;;; ------------------------------------------------------------
;;; Pages
;;; ------------------------------------------------------------

(define-regex-page cheque (("financial/cheque/" (cheque-kind "receivable|payable")))
    ((search string)
     (status string)
     (start  integer)
     (id     integer chk-cheque-id))
  (with-auth ("configuration")
    (no-cache)
    (check-cheque-accounts)
    (if (every #'validp (parameters *page*))
        (let* ((filter (parameters->plist status search))
               (page-title (conc "Επιταγές » " (cheque-kind-label cheque-kind) " » Κατάλογος"))
               (cheque-table (make-instance 'cheque-table
                                            :id "cheque-table"
                                            :subpage cheque-kind
                                            :op 'catalogue
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
                                          '(catalogue)
                                          '(catalogue details update delete)))
                         (display cheque-table
                                  :selected-id (val* id)
                                  :selected-data nil
                                  :start (val* start)))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (cheque cheque-kind) (val search))
                         (cheque-filters cheque-kind)
                         (cheque-status-filters cheque-kind (val status) (val search)))
                   (footer)))))
        (see-other (notfound)))))

(define-regex-page cheque/create (("financial/cheque/" (cheque-kind  "receivable|payable") "/create"))
    ((search     string)
     (bank       string  chk-bank-title)
     (due-date   date    chk-date)
     (company    string  chk-company-title)
     (amount     float   chk-amount)
     (status     string  chk-cheque-status)
     (account-id integer chk-acc-id))
  (with-auth ("configuration")
    (no-cache)
    (check-cheque-accounts)
    (let ((filter (parameters->plist search status))
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
                                  '(details create update delete)))
               (with-form (actions/financial/cheque/create cheque-kind :search (val* search))
                 (cheque-data-form cheque-kind
                                   'create
                                   :filter filter
                                   :data (parameters->plist bank
                                                            company
                                                            due-date
                                                            amount
                                                            status
                                                            account-id)
                                   :styles (parameters->styles bank
                                                               company
                                                               due-date
                                                               amount
                                                               status
                                                               account-id)))
               (footer)))))))

(define-regex-page cheque/update (("financial/cheque/" (cheque-kind "receivable|payable") "/update"))
    ((search     string)
     (id         integer chk-cheque-id     t)
     (bank       string  chk-bank-title)
     (company    string  chk-company-title)
     (due-date   date    chk-date)
     (status     string  chk-cheque-status)
     (amount     float   chk-amount)
     (account-id integer chk-acc-id))
  (with-auth ("configuration")
    (no-cache)
    (check-cheque-accounts)
    (if (validp id)
        (let ((page-title (conc "Επιταγές » " (cheque-kind-label cheque-kind) " » Επεξεργασία"))
              (filter (parameters->plist status search)))
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
                                      '(create update))
                         (cheque-notifications))
                   (with-form (actions/financial/cheque/update cheque-kind
                                                               :id (val id)
                                                               :search (val search))
                     (cheque-data-form cheque-kind
                                       'update
                                       :id (val id)
                                       :filter filter
                                       :data (plist-union (parameters->plist bank
                                                                             company
                                                                             due-date
                                                                             amount
                                                                             status
                                                                             account-id)
                                                          (get-cheque-plist (val id)))
                                       :styles (parameters->styles bank
                                                                   company
                                                                   due-date
                                                                   amount
                                                                   status
                                                                   account-id)))
                   (footer)))))
        (see-other (error-page)))))

(define-regex-page cheque/details (("financial/cheque/" (cheque-kind "(receivable|payable)") "/details"))
    ((search string)
     (status string)
     (id     integer chk-cheque-id t))
  (with-auth ("configuration")
    (no-cache)
    (check-cheque-accounts)
    (if (validp id)
        (let ((page-title (conc "Επιταγές » " (cheque-kind-label cheque-kind) " » Λεπτομέρειες"))
              (filter (parameters->plist status search)))
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
                                      '(details create)))
                   (cheque-data-form cheque-kind
                                     'details
                                     :filter filter
                                     :id (val id)
                                     :data (get-cheque-plist (val id)))))))
        (see-other (error-page)))))

(define-regex-page cheque/delete (("financial/cheque/" (cheque-kind "(receivable|payable)") "/delete"))
    ((search string)
     (status string)
     (id     integer chk-cheque-id t))
  (with-auth ("configuration")
    (no-cache)
    (check-cheque-accounts)
    (if (validp id)
        (let* ((page-title (conc "Επιταγές » " (cheque-kind-label cheque-kind) " » Διαγραφή"))
               (filter (parameters->plist status search))
               (cheque-table (make-instance 'cheque-table
                                            :op 'delete
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
                                            '(catalogue create delete))
                               (with-form (actions/financial/cheque/delete cheque-kind
                                                                           :id (val id)
                                                                           :search (val* search)
                                                                           :status (val status))
                                 (display cheque-table
                                          :selected-id (val id))))
                         (:div :id "sidebar" :class "sidebar grid_2"
                               (searchbox (cheque cheque-kind) (val search))
                               (cheque-filters cheque-kind)
                               (cheque-status-filters cheque-kind (val status) (val search))))
                   (footer)))))
        (see-other (error-page)))))

(defun cheque-data-form (cheque-kind op &key filter id data styles)
  (let* ((revenues-p (string-equal cheque-kind "receivable"))
         (disabledp (eql op 'details))
         (tree (if (eql op 'create)
                   (account-tree revenues-p)
                   nil)))
    (when (eql op 'create)
      (push (root (make-instance 'account-radio-tree
                                 :root-id (if revenues-p
                                              *invoice-receivable-account*
                                              *invoice-payable-account*)
                                 :filter revenues-p))
            (children (root tree))))
    (flet ((label+textbox (name label &optional extra-styles)
             (with-html
               (label name label)
               (textbox name
                        :value (getf data (make-keyword name))
                        :disabledp disabledp
                        :style (conc (getf styles (make-keyword name))
                                     " " extra-styles)))))
      (with-html
        (:div :id "cheque-data-form" :class "data-form"
              (:div :class "grid_6"
                    (:div :class "data-form-first"
                          (label+textbox 'company "Εταιρία" "ac-company"))
                    (label+textbox 'bank "Τράπεζα" "ac-bank")
                    (label+textbox 'due-date "Ημερομηνία πληρωμής" "datepicker")
                    (label+textbox 'amount "Ποσό")
                    (label 'status "Κατάσταση")
                    (dropdown 'status
                              *cheque-statuses*
                              :selected (or (getf data :status) *default-cheque-status*)
                              :disabledp disabledp)
                    (:div :class "data-form-buttons"
                          (if disabledp
                              (cancel-button (apply #'cheque cheque-kind :id id filter)
                                             "Επιστροφή στον Κατάλογο Επιταγών")
                              (progn
                                (ok-button (if (eql op 'update) "Ανανέωση" "Δημιουργία"))
                                (cancel-button (apply #'cheque cheque-kind :id id filter)
                                               "Άκυρο")))))
              (:div :class "grid_6 data-form-first"
                    (label 'account (conc "Λογαριασμός " (if revenues-p "πίστωσης" "χρέωσης")))
                    ;; Display the tree. If needed, preselect the first account of the tree.
                    (display tree :selected-id (or (getf data :account-id)
                                                   (key (first (children (root tree))))))))))))
