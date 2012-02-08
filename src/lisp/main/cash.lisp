(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass cash-page (regex-page page-family-mixin)
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
;;; Checks
;;; ----------------------------------------------------------------------

(defun check-cash-accounts ()
  (unless (and *cash-acc-id*
               *revenues-root-acc-id*
               *expenses-root-acc-id*)
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
  ((header-labels  :initform '("" "Ημερομηνία" "Εταιρία" "Περιγραφή" "Ποσό" "Λογαριασμός" "" ""))
   (kind :accessor kind :initarg :kind)
   (paginator :initform (make-instance 'cash-paginator
                                       :css-class "paginator")))
  (:default-initargs :item-class 'cash-tx-row :id "cash-tx-table"))

(defmethod get-records ((table cash-tx-table))
  (flet ((acc-filter (kind)
           (cond ((string-equal kind "revenue")
                  `(debit-acc-id ,*cash-acc-id*))
                 ((string-equal kind "expense")
                  `(credit-acc-id ,*cash-acc-id*))
                 (t
                  (error "internal error in acc-filter"))))
         (acc-join (kind)
           (cond ((string-equal kind "revenue")
                  'tx.credit-acc-id)
                 ((string-equal kind "expense")
                  'tx.debit-acc-id)
                 (t
                  (error "internal error in acc-join")))))
    (let* ((search (getf (filter table) :search))
           (since (getf (filter table) :since))
           (until (getf (filter table) :until))
           (kind (kind table))
           (base-query `(:select tx.id tx-date
                                 (:as company.title company)
                                 (:as company.id 'company-id)
                                 (:as account.title account)
                                 description amount
                         :from tx
                         :left-join company
                         :on (:= tx.company-id company.id)
                         :left-join account
                         :on (:= ,(acc-join kind) account.id)))
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
      (let ((sql `(:order-by (,@base-query :where
                                           (:and (:= ,@(acc-filter kind))
                                                 ,@where))
                             (:desc tx-date) account company description)))
        (query (sql-compile sql)
               :plists)))))


;;; rows

(defclass cash-tx-row (tx-row)
  ())

(defmethod selector ((row cash-tx-row) selected-p)
  (let* ((id (key row))
         (table (collection row))
         (pg (paginator table))
         (filter (filter table))
         (kind (kind table))
         (start (start-index table)))
    (html ()
      (:a :href (if selected-p
                    (apply #'cash kind :start (page-start pg (index row) start) filter)
                    (apply #'cash kind :id id filter))
          (selector-img selected-p)))))

(defmethod payload ((row cash-tx-row) enabled-p)
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
                         '(tx-date description amount account)))))

(defmethod controls ((row cash-tx-row) controls-p)
  (let* ((id (key row))
         (table (collection row))
         (filter (filter table))
         (kind (kind table)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button :href (apply #'cash kind :id id filter)))
        (list nil nil))))


;;; paginator

(defclass cash-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg cash-paginator) start)
  (let ((table (table pg)))
   (apply #'cash (kind table) :start start (filter table))))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun revenues-p (kind)
  (string-equal kind "revenue"))

(defun cash-page-title (kind)
  (cond ((string-equal kind "revenue") "Έσοδα")
        ((string-equal kind "expense") "Έξοδα")
        (t (error "Internal error in cash-page-title"))))

(defun cash-top-actions (op kind id filter)
  (let ((new-cash-label (conc "Νέο "
                              (cond ((string-equal kind "revenue") "Έσοδo")
                                    ((string-equal kind "expense") "Έξοδo")
                                    (t (error "Internal error in cash-top-actions"))))))
    (top-actions (make-instance 'menu
                                :spec `((catalogue
                                         ,(html ()
                                            (:a :href (apply #'cash kind :id id filter)
                                                (:img :src "/scrooge/img/application_view_list.png")
                                                "Κατάλογος")))
                                        (create
                                         ,(html ()
                                            (:a :href (apply #'cash/create kind filter)
                                                (:img :src "/scrooge/img/add.png")
                                                (str new-cash-label)))))
                                :css-class "hmenu"
                                :disabled (cond ((member op '(:catalogue :delete))
                                                 '(catalogue))
                                                ((eql op :create)
                                                 '(create))
                                                (t
                                                 nil)))
                 (searchbox #'(lambda (&rest args)
                                (apply #'actions/cash/search kind args))
                            #'(lambda (&rest args)
                                (apply #'cash kind :id id args))
                            filter
                            "ac-company"))))

(defun cash-actions (op kind id filter)
  (actions-menu (make-menu-spec
                 (action-anchors/crud+details (apply #'cash/details kind :id id filter)
                                              (apply #'cash/update kind :id id filter)
                                              (apply #'cash/delete kind :id id filter)))
                (enabled-actions/crud+details op id)))

(defun cash-filters (kind filter)
  (filter-area (html ()
                 (:div :class "grid_8 alpha"
                       (display (filter-navbar `((revenue ,(apply #'cash "revenue" filter) "Έσοδα")
                                                 (expense ,(apply #'cash "expense" filter) "Έξοδα"))
                                               kind))))
               (html ()
                 (:div :class "grid_4 omega"
                       (display (datebox (lambda (&rest args)
                                           (apply #'cash kind args))
                                         filter))))))

(defpage cash-page actions/cash/search
    (("actions/cash/" (kind "(expense|revenue)") "/search") :request-type :get)
    ((search string))
  (with-db ()
    (let* ((filter (params->filter))
           (records (get-records (make-instance 'cash-tx-table
                                                :kind kind
                                                :filter filter))))
      (if (or (not records)
              (and records (cdr records)))
          (see-other (apply #'cash kind filter))
          (see-other (apply #'cash/details kind :id (getf (first records) :id) filter))))))



;;; ------------------------------------------------------------
;;; Cash form
;;; ------------------------------------------------------------

(defclass cash-form (crud-form/plist)
  ((kind :accessor kind :initarg :kind)))

(defmethod display ((form cash-form) &key styles)
  (let* ((revenues-p (revenues-p (kind form)))
         (disabled (eql (op form) :details))
         (record (record form))
         (lit (label-input-text disabled record styles))
         (tree (make-instance 'rev/exp-account-tree
                              :disabled disabled
                              :root-key (if revenues-p
                                            *revenues-root-acc-id*
                                            *expenses-root-acc-id*)
                              :filter (list :debit-p (not revenues-p)))))
    (with-html
      (:div :id "cash-data-form" :class "data-form"
            (:div :class "grid_5 prefix_1 alpha"
                  (display lit 'tx-date "Ημερομηνία":extra-styles "datepicker" :value (today))
                  (display lit 'description "Περιγραφή")
                  (display lit 'company "Εταιρία" :extra-styles "ac-company")
                  (display lit 'amount "Ποσό")
                  (unless disabled
                    (htm (:div :class "data-form-buttons"
                               (ok-button :body (if (eql (op form) :update)
                                                    "Ανανέωση"
                                                    "Δημιουργία"))
                               (cancel-button (cancel-url form)
                                              :body "Άκυρο")))))
            ;; Display the tree. If needed, preselect the first account of the tree.
            (let ((account-id (getf record :account-id))
                  (recv/pay-acc-id (if revenues-p
                                       *invoice-receivable-acc-id*
                                       *invoice-payable-acc-id*)))
              (htm (:div :class "grid_5 omega"
                         (:h3 (str (conc "Λογαριασμός "
                                         (if revenues-p "πίστωσης" "χρέωσης"))))
                         (:h4 (str "Έναντι ανοιχτού λογαριασμού"))
                         (input-radio 'account-id
                                      recv/pay-acc-id
                                      (html ()
                                        (str (title (get-dao 'account recv/pay-acc-id))))
                                      :disabled disabled
                                      :checked (or (eql recv/pay-acc-id account-id)
                                                   (not account-id)))
                         (:h4 (str (conc "Απ' ευθείας χρέωση σε λογαριασμό "
                                         (if revenues-p "εσόδων" "εξόδων"))))
                         (display tree :key (or (getf record :account-id)
                                                (getf record (if revenues-p
                                                                 :credit-acc-id
                                                                 :debit-acc-id)))))))
            (clear)))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defpage cash-page cash (("cash/" (kind "(expense|revenue)")))
    ((search string)
     (start  integer)
     (since  date)
     (until  date)
     (id     integer chk-tx-id))
  (with-view-page
    (check-cash-accounts)
    (let* ((op :catalogue)
           (filter (params->filter))
           (page-title (conc (cash-page-title kind) " » Κατάλογος"))
           (cash-tx-table (make-instance 'cash-tx-table
                                         :kind kind
                                         :op op
                                         :filter filter
                                         :start-index (val start))))
      ;; if id exists and is not found among records, ignore search term
      (when (and (val id)
                 (not (find (val id) (rows cash-tx-table) :key #'key)))
        (let ((tx (get-dao 'tx (val id))))
          (see-other (cash (cond ((eql (debit-acc-id tx) *cash-acc-id*)
                                  "revenue")
                                 ((eql (credit-acc-id tx) *cash-acc-id*)
                                  "expense")
                                 (t (error 'bad-request-error)))
                           :id (val id)))))
      ;; otherwise continue as usually
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cash)
               (cash-top-actions op kind (val id) filter)
               (cash-filters kind filter)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" (str page-title))
                           (cash-actions op kind (val id) filter)
                           (display cash-tx-table
                                    :key (val id)
                                    :payload nil)))
               (footer)))))))

(defpage cash-page cash/details (("cash/" (kind "(expense|revenue)") "/details"))
    ((search string)
     (since  date)
     (until  date)
     (id     integer chk-tx-id t))
  (with-view-page
    (check-cash-accounts)
    (let* ((op :details)
           (filter (params->filter))
           (cash-form (make-instance 'cash-form
                                     :kind kind
                                     :op op
                                     :record (get-record 'tx (val id))
                                     :cancel-url (apply #'cash kind :id (val id) filter)))
           (page-title (conc (cash-page-title kind) " » Λεπτομέρειες")))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cash)
               (cash-top-actions op kind (val id) filter)
               (:div :class "grid_12"
                     (:div :id "cash-window" :class "window"
                           (:div :class "title" "Λεπτομέρειες")
                           (cash-actions op kind (val id) filter)
                           (display cash-form)))
               (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage cash-page cash/create
    (("cash/" (kind "(expense|revenue)") "/create"))
    ((search      string)
     (since       date)
     (until       date)
     (tx-date     date)
     (company     string  chk-company-title)
     (description string)
     (amount      float   chk-amount)
     (account-id  integer chk-acc-id))
  (with-view-page
    (check-cash-accounts)
    (let* ((op :create)
           (filter (params->filter))
           (cash-form (make-instance 'cash-form
                                     :kind kind
                                     :op op
                                     :cancel-url (apply #'cash kind filter)))
           (page-title (conc (cash-page-title kind) " » Δημιουργία")))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cash)
               (cash-top-actions op kind nil filter)
               (:div :class "grid_12"
                     (:div :id "cash-window" :class "window"
                           (:div :class "title" (str page-title))
                           (cash-actions op kind nil filter)
                           (notifications)
                           (with-form (actions/cash/create kind
                                                           :search (val search)
                                                           :since (val since)
                                                           :until (val until))
                             (display cash-form :payload (params->payload)
                                                :styles (params->styles)))))
               (footer)))))))

(defpage cash-page actions/cash/create
    (("actions/cash/" (kind "(expense|revenue)") "/create") :request-type :post)
    ((search      string)
     (since       date)
     (until       date)
     (tx-date     date)
     (company     string  chk-company-title t)
     (description string)
     (amount      float   chk-amount t)
     (account-id  integer chk-acc-id t))
  (with-controller-page (cash/create kind)
    (check-cash-accounts)
    (let* ((company-id (company-id (val company)))
           (debit-acc-id (if (string-equal kind "revenue")
                             *cash-acc-id*
                             (val account-id)))
           (credit-acc-id (if (string-equal kind "revenue")
                              (val account-id)
                              *cash-acc-id*))
           (new-tx (make-instance 'tx
                                  :tx-date (val tx-date)
                                  :description (val description)
                                  :company-id company-id
                                  :amount (val amount)
                                  :credit-acc-id credit-acc-id
                                  :debit-acc-id debit-acc-id
                                  :auto t)))
      (insert-dao new-tx)
      (see-other (apply #'cash/details kind :id (tx-id new-tx)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage cash-page cash/update
    (("cash/" (kind "(expense|revenue)") "/update"))
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
    (check-cash-accounts)
    (let* ((op :update)
           (filter (params->filter))
           (cash-form (make-instance 'cash-form
                                     :kind kind
                                     :op op
                                     :record (get-record 'tx (val id))
                                     :cancel-url (apply #'cash/details kind :id (val id) filter)))
           (page-title (conc (cash-page-title kind) " » Επεξεργασία")))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cash)
               (cash-top-actions op kind (val id) filter)
               (:div :class "grid_12"
                     (:div :id "cash-window" :class "window"
                           (:div :class "title" (str page-title))
                           (cash-actions op kind (val id) filter)
                           (notifications)
                           (with-form (actions/cash/update kind
                                                           :id (val id)
                                                           :search (val search)
                                                           :since (val since)
                                                           :until (val until))
                             (display cash-form :payload (params->payload)
                                                :styles (params->styles)))))
               (footer)))))))

(defpage cash-page actions/cash/update
    (("actions/cash/" (kind "(expense|revenue)") "/update") :request-type :post)
    ((search      string)
     (since       date)
     (until       date)
     (id          integer chk-tx-id         t)
     (tx-date     date)
     (description string)
     (company     string  chk-company-title)
     (amount      float   chk-amount)
     (account-id  integer chk-acc-id))
  (with-controller-page (cash/update kind)
    (check-cash-accounts)
    (let ((company-id (company-id (val company)))
          (debit-acc-id (if (string-equal kind "revenue")
                            *cash-acc-id*
                            (val account-id)))
          (credit-acc-id (if (string-equal kind "revenue")
                             (val account-id)
                             *cash-acc-id*)))
      (execute (:update 'tx :set
                        'tx-date (val tx-date)
                        'description (val description)
                        'company-id company-id
                        'amount (val amount)
                        'debit-acc-id debit-acc-id
                        'credit-acc-id credit-acc-id
                        :where (:= 'id (val id))))
      (see-other (apply #'cash/details kind :id (val id) (params->filter))))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage cash-page cash/delete
    (("cash/" (kind "(expense|revenue)") "/delete"))
    ((search string)
     (since  date)
     (until  date)
     (id     integer chk-tx-id t))
  (with-view-page
    (check-cash-accounts)
    (let* ((op :delete)
           (filter (params->filter))
           (page-title (conc (cash-page-title kind) " » Διαγραφή"))
           (cash-tx-table (make-instance 'cash-tx-table
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
               (main-navbar 'cash)
               (cash-top-actions op kind (val id) filter)
               (cash-filters kind filter)
               (:div :class "grid_12"
                     (:div :id "cash-window" :class "window"
                           (:div :class "title" (str page-title))
                           (cash-actions op kind (val id) filter)
                           (with-form (actions/cash/delete kind
                                                           :id (val id)
                                                           :search (val search)
                                                           :since (val since)
                                                           :until (val until))
                             (display cash-tx-table
                                      :key (val id)))))
               (footer)))))))

(defpage cash-page actions/cash/delete
    (("actions/cash/" (kind "(expense|revenue)") "/delete") :request-type :post)
    ((search string)
     (since  date)
     (until  date)
     (id     integer chk-tx-id t))
  (with-controller-page (cash/delete)
    (check-cash-accounts)
    (delete-dao (get-dao 'tx (val id)))
    (see-other (apply #'cash kind (params->filter)))))
