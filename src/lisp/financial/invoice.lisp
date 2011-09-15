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
    :initform '(date company description amount))
   (filter-parameter-names
    :allocation :class
    :initform '(search state))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))
   (messages
    :allocation :class
    :reader messages
    :initform
    '((company (:company-title-unknown
                "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"))
      (amount (:non-positive-amount
               "Το ποσό της συναλλαγής πρέπει να είναι θετικός αριθμός"
               :parse-error
               "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες"))
      (account-id (:acc-id-null
                   "Δεν έχετε επιλέξει λογαριασμό"))
      (date (:parse-error
             "Η ημερομηνία της συναλλαγής είναι άκυρη"))))))



;;; ----------------------------------------------------------------------
;;; Checks
;;; ----------------------------------------------------------------------

(defun check-invoice-accounts ()
  (unless (and *invoice-receivable-account*
               *invoice-payable-account*
               *revenues-root-account*
               *expenses-root-account*)
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
;;; Invoice form
;;; ----------------------------------------------------------------------

(defclass invoice-form (crud-form/plist)
  ())

(defun invoice-data-form (invoice-kind op &key id data styles filter)
  (let ((disabled (eql op :details))
        (record (record form))
        (tree (make-account-radio-tree (string-equal invoice-kind "receivable"))))
    (flet ((label-input-text (name label &optional extra-styles)
             (with-html
               (label name label)
               (input-text name
                           :value (getf data (make-keyword name))
                           :disabled disabled
                           :css-class (conc (getf styles (make-keyword name))
                                            " " extra-styles)))))
      (with-html
        (:div :id "invoice-data-form" :class "data-form"
              (:div :class "grid_6 alpha"
                    (label-input-text 'date "Ημερομηνία" "datepicker")
                    (label-input-text 'description "Περιγραφή")
                    (label-input-text 'company "Εταιρία" "ac-company")
                    (label-input-text 'amount "Ποσό")
                    (:div :class "data-form-buttons"
                          (if disabled
                              (cancel-button (cancel-url form)
                                             :body "Επιστροφή στον Κατάλογο Συναλλαγών Μετρητών")
                              (progn
                                (ok-button :body (if (eql op :update) "Ανανέωση" "Δημιουργία"))
                                (cancel-button (cancel-url form)
                                               :body "Άκυρο")))))
              (:div :class "grid_6 omega"
                    (label 'account "Λογαριασμός")
                    ;; Display the tree. If needed, preselect the first account of the tree.
                    (display tree :selected-id (or (getf data :account-id)
                                                   (key (first (children (root tree))))))))))))


;;; ----------------------------------------------------------------------
;;; Invoice transactions table
;;; ----------------------------------------------------------------------

(defclass invoice-tx-table (tx-table)
  ((subpage :accessor subpage :initarg :subpage)
   (paginator :initform (make-instance 'scrooge-paginator
                                       :id "invoice-tx-paginator"
                                       :css-class "paginator")))
  (:default-initargs :item-class 'invoice-tx-row))

(defmethod initialize-instance :after ((table invoice-tx-table) &key)
  (let ((invoice-kind (subpage table)))
    (setf (urlfn (paginator table))
          (lambda (&rest args)
            (apply #'invoice invoice-kind args)))))

(defmethod get-records ((table invoice-tx-table))
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

(defmethod selector ((row invoice-tx-row) enabled-p)
  (let* ((id (key row))
         (table (collection row))
         (pg (paginator table))
         (invoice-kind (subpage table))
         (filter (filter table))
         (start (start-index table)))
    (html ()
      (:a :href (if enabled-p
                    (apply #'invoice invoice-kind :start (page-start pg (index row) start) filter)
                    (apply #'invoice invoice-kind :id id filter))
          (selector-img enabled-p)))))

(defmethod payload ((row invoice-tx-row) enabled-p)
  (let ((record (record row)))
    (mapcar (lambda (name)
              (make-instance 'textbox
                             :name name
                             :value (getf record (make-keyword name))
                             :disabled (not enabled-p)))
            '(date company description amount))))

(defmethod controls ((row invoice-tx-row) enabled-p)
  (let* ((id (key row))
         (table (collection row))
         (invoice-kind (subpage table))
         (filter (filter table)))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button :href (apply #'invoice invoice-kind :id id filter)))
        (list nil nil))))



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
            (navbar spec
                    :id "invoice-filters"
                    :css-class "vnavbar"
                    :active (intern (string-upcase invoice-kind)))))))

(defun invoice-menu (invoice-kind id filter disabled)
  (menu (crud-actions-spec (apply #'invoice        invoice-kind :id id filter)
                           (apply #'invoice/create invoice-kind filter)
                           (apply #'invoice/update invoice-kind :id id filter)
                           (apply #'invoice/delete invoice-kind :id id filter))
        :id "invoice-actions"
        :css-class "hmenu actions"
        :disabled disabled))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage invoice-page invoice (("invoice/" (invoice-kind "(receivable|payable)")))
    ((search    string)
     (start     integer)
     (id        integer chk-tx-id))
  (with-view-page
    (check-invoice-accounts)
    (let* ((filter (params->filter))
           (page-title (conc "Τιμολόγια » " (invoice-kind-label invoice-kind) " » Κατάλογος"))
           (invoice-tx-table (make-instance 'invoice-tx-table
                                            :id "invoice-tx-table"
                                            :subpage invoice-kind
                                            :op :read
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
                                       '(:read)
                                       '(:read :details :update :delete)))
                     (display invoice-tx-table
                              :selected-id (val id)
                              :selected-data nil
                              :start (val start)))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (invoice invoice-kind) (val search))
                     (invoice-filters invoice-kind (val search)))
               (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage invoice-page invoice/create (("invoice/" (invoice-kind  "(receivable|payable)") "/create"))
    ((search      string)
     (date        date)
     (company     string chk-company-title*)
     (description string)
     (amount      float   chk-amount)
     (account-id  integer chk-acc-id))
  (with-view-page
    (check-invoice-accounts)
    (let ((filter (params->filter))
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
                                   '(:create :update :delete))
                     (invoice-notifications)
                     (with-form (actions/invoice/create invoice-kind :search (val search))
                       (invoice-data-form invoice-kind :create
                                          :filter filter
                                          :data (params->payload)
                                          :styles (params->styles))))
               (footer)))))))

(defpage invoice-page actions/invoice/create
    (("actions/invoice/" (invoice-kind "(receivable|payable)") "/create")
     :request-type :post)
    ((search      string)
     (date        date)
     (company     string  chk-company-title*)
     (description string)
     (amount      float   chk-amount)
     (account-id  integer chk-acc-id t))
  (with-controller-page (invoice/update)
    (check-invoice-accounts)
    (let* ((company-id (company-id (val company))) ;; using val (accept null values)
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
      (insert-dao new-tx)
      (see-other (invoice invoice-kind :id (id new-tx) :search (val search))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage invoice-page invoice/update
    (("invoice/" (invoice-kind "(receivable|payable)") "/update"))
    ((search      string)
     (id          integer chk-tx-id t)
     (date        date)
     (company     string chk-company-title*)
     (description string)
     (amount      float   chk-amount)
     (account-id  integer chk-acc-id))
  (with-view-page
    (check-invoice-accounts)
    (let ((filter (params->filter))
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
                                   '(:create :update :delete))
                     (with-form (actions/invoice/update invoice-kind
                                                                  :id (val id)
                                                                  :search (val search))
                       (invoice-data-form invoice-kind :update
                                          :id (val id)
                                          :filter filter
                                          :data (params->payload)
                                          :styles (params->styles))))
               (footer)))))))

(defpage invoice-page actions/invoice/update
    (("actions/invoice/" (invoice-kind "(receivable|payable)") "/update") :request-type :post)
    ((search      string)
     (id          integer chk-tx-id t)
     (date        date)
     (description string)
     (company     string  chk-company-title*)
     (amount      float   chk-amount)
     (account-id  integer chk-acc-id))
  (with-view-page
    (check-invoice-accounts)
    (let ((company-id (company-id (val company))) ;; using val (accept null values)
          (debit-acc-id (if (string-equal invoice-kind "receivable")
                            *invoice-receivable-account*
                            (val account-id)))
          (credit-acc-id (if (string-equal invoice-kind "receivable")
                             (val account-id)
                             *invoice-payable-account*)))
      (execute (:update 'tx :set
                        'tx-date (val date)
                        'description (val description)
                        'company-id company-id
                        'amount (val amount)
                        'debit-acc-id debit-acc-id
                        'credit-acc-id credit-acc-id
                        :where (:= 'id (val id))))
      (see-other (invoice invoice-kind :id (val id))))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage invoice-page invoice/delete
    (("invoice/" (invoice-kind "(receivable|payable)") "/delete"))
    ((id     integer chk-project-id t)
     (search string))
  (with-view-page
    (check-invoice-accounts)
    (let* ((filter (params->filter))
           (page-title (conc "Τιμολόγια » " (invoice-kind-label invoice-kind) " » Διαγραφή"))
           (invoice-tx-table (make-instance 'invoice-tx-table
                                            :op :delete
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
                                   '(:read :delete))
                     (with-form (actions/invoice/delete invoice-kind
                                                        :id (val id)
                                                        :search (val search))
                       (display invoice-tx-table
                                :selected-id (val id))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (invoice invoice-kind) (val search))
                     (invoice-filters invoice-kind (val search)))
               (footer)))))))

(defpage invoice-page actions/invoice/delete
    (("actions/invoice/" (invoice-kind "(receivable|payable)") "/delete") :request-type :post)
    ((id     integer chk-tx-id t)
     (search string))
  (with-controller-page (invoice/delete)
    (check-invoice-accounts)
    (delete-dao (get-dao 'tx (val id)))
    (see-other (invoice invoice-kind :search (val search)))))


;;; UPDATE WTF
;; (plist-union (params->payload)
;;              (subst :account-id
;;                     acc-keyword
;;                     (tx-record (val id))))
