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
  ((header-labels  :initform '("" "Ημερομηνία" "Εταιρία" "Περιγραφή" "Ποσό" "" ""))
   (kind :accessor kind :initarg :kind)
   (paginator :initform (make-instance 'scrooge-paginator
                                       :css-class "paginator")))
  (:default-initargs :item-class 'cash-tx-row))

(defmethod get-records ((table cash-tx-table))
  (flet ((acc-kind (kind)
           (cond ((string-equal kind "revenue")
                  `(debit-acc-id ,*cash-acc-id*))
                 ((string-equal kind "expense")
                  `(credit-acc-id ,*cash-acc-id*))
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
        (push `(:or (:ilike description ,(ilike search))
                    (:ilike company.title ,(ilike search)))
              where))
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
                         '(tx-date description amount)))))

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

(defun cash-actions (op kind id filter)
  (actions-menu (crud+details-actions-spec (apply #'cash/create kind filter)
                                           (apply #'cash/details kind :id id filter)
                                           (apply #'cash/update kind :id id filter)
                                           (apply #'cash/delete kind :id id filter))
                (crud+details-actions-enabled/disabled op id)))

(defun cash-filters (kind filter)
  (with-html
    (:div :class "filters"
          (filters-navbar `((revenue ,(apply #'cash "revenue" filter) "Έσοδα")
                            (expense ,(apply #'cash "expense" filter) "Έξοδα"))
                          kind)
          (datebox (lambda (&rest args)
                     (apply #'cash kind args))
                   filter))))

(defun cash-subnavbar (op kind filter)
  (with-html
    (:div :class "section-subnavbar grid_12"
          (if (member op '(:catalogue :delete))
              (cash-filters kind filter)
              (htm (:div :class "options"
                         (:ul (:li (:a :href (apply #'cash kind filter)
                                       "Κατάλογος"))))))

          (searchbox #'(lambda (&rest args)
                         (apply #'cash kind args))
                     filter))))



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
                                           (if revenues-p "εσόδων" "εξόδων")))
                  ;; Display the tree. If needed, preselect the first account of the tree.
                  (display tree :key (or (getf record (if revenues-p
                                                          :credit-acc-id
                                                          :debit-acc-id))
                                         (root-key tree))))))))



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
                                         :id "cash-tx-table"
                                         :kind kind
                                         :op op
                                         :filter filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cash)
               (cash-subnavbar op kind filter)
               (:div :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cash-actions op kind (val id) filter)
                     (display cash-tx-table
                              :key (val id)
                              :payload nil
                              :start (val start)))
               (footer)))))))

(defpage cash-page cash/details (("cash/" (kind "(expense|revenue)") "/details"))
    ((search      string)
     (since       date)
     (until       date)
     (id          integer chk-tx-id t))
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
               (cash-subnavbar op kind filter)
               (:div :id "cash-window" :class "window grid_12"
                     (:div :class "title" "Λεπτομέρειες")
                     (cash-actions op kind (val id) filter)
                     (display cash-form))
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
                                     :record nil
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
               (cash-subnavbar op kind filter)
               (:div :id "cash-window" :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cash-actions op kind nil filter)
                     (notifications)
                     (with-form (actions/cash/create kind
                                                     :search (val search)
                                                     :since (val since)
                                                     :until (val until))
                       (display cash-form :payload (params->payload)
                                          :styles (params->styles))))
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
                                     :cancel-url (apply #'cash kind :id (val id) filter)))
           (page-title (conc (cash-page-title kind) " » Επεξεργασία")))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cash)
               (cash-subnavbar op kind filter)
               (:div :id "cash-window" :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cash-actions op kind (val id) filter)
                     (notifications)
                     (with-form (actions/cash/update kind
                                                     :id (val id)
                                                     :search (val search)
                                                     :since (val since)
                                                     :until (val until))
                       (display cash-form :payload (params->payload)
                                          :styles (params->styles))))
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
               (cash-subnavbar op kind filter)
               (:div :id "cash-window" :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cash-actions op kind (val id) filter)
                     (with-form (actions/cash/delete kind
                                                     :id (val id)
                                                     :search (val search)
                                                     :since (val since)
                                                     :until (val until))
                       (display cash-tx-table
                                :key (val id))))
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
