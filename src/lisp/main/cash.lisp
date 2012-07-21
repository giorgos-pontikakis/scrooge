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
  ((direction     :accessor direction                                                                                     :initarg :direction)
   (paginator     :initform (make-instance 'cash-paginator
                                           :css-class "paginator"))
   (header-labels :initform '("" "Ημερομηνία" "Εταιρία" "Περιγραφή" "Λογαριασμός" "Ποσό" "" "")))
  (:default-initargs :item-class 'cash-tx-row :id "cash-tx-table"))

(defmethod get-records ((table cash-tx-table))
  (flet ((acc-filter (direction)
           (if (incoming-p direction)
               `(debit-acc-id ,(account-id 'cash-account))    ; revenue
               `(credit-acc-id ,(account-id 'cash-account)))) ; expense
         (acc-join (direction)
           (if (incoming-p direction)
               'tx.credit-acc-id   ; revenue
               'tx.debit-acc-id))) ; expense
    (let* ((search (getf (filter table) :search))
           (since (getf (filter table) :since))
           (until (getf (filter table) :until))
           (direction (direction table))
           (base-query `(:select tx.id tx-date
                          (:as company.title company)
                          (:as company.id 'company-id)
                          (:as account.title account)
                          description amount
                          :from tx
                          :left-join company
                          :on (:= tx.company-id company.id)
                          :left-join account
                          :on (:= ,(acc-join direction) account.id)
                          :left-join 'cheque-event
                          :on (:= 'cheque-event.tx-id 'tx.id)))
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
                                           (:and (:= ,@(acc-filter direction))
                                                 (:is-null 'cheque-event.cheque-id)
                                                 ,@where))
                             (:desc tx-date) account company description)))
        (query (sql-compile sql)
               :plists)))))

(defmethod actions ((tbl cash-tx-table) &key)
  (let* ((tx-id (selected-key tbl))
         (direction (direction tbl))
         (filter (filter tbl))
         (hrefs (if tx-id
                    (list :details (apply #'cash/details direction :tx-id tx-id filter)
                          :delete (apply #'cash/delete direction :tx-id tx-id filter))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))

(defmethod filters ((tbl cash-tx-table))
  (let ((direction (direction tbl))
        (filter (filter tbl)))
    (filter-area (filter-navbar `((incoming ,(apply #'cash "incoming" filter) "Έσοδα")
                                  (outgoing ,(apply #'cash "outgoing" filter) "Έξοδα"))
                                :active direction
                                :id "direction-navbar")
                 (datebox (lambda (&rest args)
                            (apply #'cash direction args))
                          filter))))



;;; rows

(defclass cash-tx-row (tx-row)
  ())

(defmethod selector ((row cash-tx-row) selected-p)
  (let* ((tx-id (key row))
         (table (collection row))
         (pg (paginator table))
         (filter (filter table))
         (direction (direction table))
         (start (start-index table)))
    (html ()
      (:a :href (if selected-p
                    (apply #'cash direction :start (page-start pg (index row) start) filter)
                    (apply #'cash direction :tx-id tx-id filter))
        (selector-img selected-p)))))

(defmethod payload ((row cash-tx-row) enabled-p)
  (let ((record (record row)))
    (list (make-instance 'textbox
                         :name 'tx-date
                         :value (getf record :tx-date)
                         :disabled (not enabled-p))
          (html ()
            (:a :href (company/details :company-id (getf record :company-id))
              (str (getf record :company))))
          (make-instance 'textbox
                         :name 'description
                         :value (getf record :description)
                         :disabled (not enabled-p))
          (make-instance 'textbox
                         :name 'account
                         :value (getf record :account)
                         :disabled (not enabled-p))
          (make-instance 'textbox
                         :name 'amount
                         :value (fmt-amount (getf record :amount))
                         :disabled (not enabled-p)))))

(defmethod controls ((row cash-tx-row) controls-p)
  (let* ((tx-id (key row))
         (table (collection row))
         (filter (filter table))
         (direction (direction table)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button :href (apply #'cash direction :tx-id tx-id filter)))
        (list nil nil))))


;;; paginator

(defclass cash-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg cash-paginator) start)
  (let ((table (table pg)))
    (apply #'cash (direction table) :start start (filter table))))



;;; ----------------------------------------------------------------------
;;; Utilities
;;; ----------------------------------------------------------------------

(defun cash-debit-acc-id (direction account-id)
  (if (incoming-p direction)
      (account-id 'cash-account)
      account-id))

(defun cash-credit-acc-id (direction account-id)
  (if (incoming-p direction)
      account-id
      (account-id 'cash-account)))

(defun cash-page-title (direction op-label)
  (conc "Μετρητά »" (if (incoming-p direction) "Έσοδα" "Έξοδα") " » " op-label))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun cash-top-actions (op)
  (let* ((direction (first *registers*))
         (new-cash-label (conc "Νέο " (if (incoming-p direction) "Έσοδo" "Έξοδo"))))
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
  ())

(defmethod display ((form cash-form) &key styles)
  (let* ((incoming-p (incoming-p (direction form)))
         (disabled (eql (op form) :details))
         (record (record form))
         (ldfn (label-datum disabled record styles))
         (revenues/expenses-root-key (revenues/expenses-root (direction form)))
         (receivable/payable-root-key (receivable/payable-root (direction form)))
         (selected-key (or (getf record :account-id)
                           (getf record (if incoming-p
                                            :credit-acc-id
                                            :debit-acc-id)))))
    (with-html
      (:div :id "cash-data-form" :class "data-form"
        (:div :class "grid_5 prefix_1 alpha"
          (display ldfn 'tx-date "Ημερομηνία":enabled-styles "datepicker"
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
        (htm (:div :class "grid_5 omega"
               (:h3 (str (conc "Λογαριασμός " (if incoming-p "πίστωσης" "χρέωσης"))))
               ;;
               (:div :class "cash-only-hidden"
                 (:h4 (str "Έναντι ανοιχτού λογαριασμού"))
                 (display (make-instance 'rev/exp-account-tree
                                         :disabled disabled
                                         :root-key receivable/payable-root-key
                                         :debit-p incoming-p
                                         :selected-key (or selected-key
                                                           receivable/payable-root-key))))
               ;;
               (:div :class "company-dependent"
                 (:h4 (str (conc "Απ' ευθείας χρέωση σε λογαριασμό "
                                 (if incoming-p "εσόδων" "εξόδων"))))
                 (display (make-instance 'rev/exp-account-tree
                                         :disabled disabled
                                         :root-key revenues/expenses-root-key
                                         :debit-p (not incoming-p)
                                         :selected-key selected-key)))))
        (clear)))))

(defmethod actions ((form cash-form) &key filter)
  (let* ((tx-id (key form))
         (direction (direction form))
         (hrefs (list :update (apply #'cash/update direction :tx-id tx-id filter)
                      :delete (apply #'cash/delete direction :tx-id tx-id filter))))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions form))))



;;; ----------------------------------------------------------------------
;;; SEARCH
;;; ----------------------------------------------------------------------

(defpage cash-page actions/cash/search
    (("actions/cash/" (direction "(incoming|outgoing)") "/search") :request-type :get)
    ((search string)
     (since date)
     (until date))
  (with-db ()
    (let* ((filter (params->filter))
           (rows (rows (make-instance 'cash-tx-table
                                      :direction direction
                                      :filter filter))))
      (if (single-item-list-p rows)
          (see-other (apply #'cash/details direction
                            :tx-id (key (first rows))
                            filter))
          (see-other (apply #'cash direction filter))))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defpage cash-page cash (("cash/" (direction "(incoming|outgoing)")))
    ((tx-id  integer chk-tx-id)
     (start  integer)
     (search string)
     (since  date)
     (until  date))
  (with-view-page
    (check-cash-accounts)
    (let* ((filter (params->filter))
           (page-title (cash-page-title direction "Κατάλογος"))
           (cash-tx-table (make-instance 'cash-tx-table
                                         :direction direction
                                         :op :catalogue
                                         :selected-key (val tx-id)
                                         :filter filter
                                         :start-index (val start))))
      ;; if tx-id exists and is not found among records, ignore search term
      (when (and (val tx-id)
                 (not (find (val tx-id) (rows cash-tx-table) :key #'key)))
        (let ((tx (get-dao 'tx (val tx-id))))
          (see-other (cash (cond ((eql (debit-acc-id tx) (account-id 'cash-account))
                                  "incoming")
                                 ((eql (credit-acc-id tx) (account-id 'cash-account))
                                  "outgoing")
                                 (t (error 'bad-request-error)))
                           :tx-id (val tx-id)))))
      ;; otherwise continue as usually
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header)
            (main-navbar 'cash)
            (cash-top-actions :catalogue)
            (filters cash-tx-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" (str page-title))
                (actions cash-tx-table)
                (display cash-tx-table)))
            (footer)))))))

(defpage cash-page cash/details (("cash/" (direction "(incoming|outgoing)") "/details"))
    ((tx-id  integer chk-tx-id t)
     (search string)
     (since  date)
     (until  date))
  (with-view-page
    (check-cash-accounts)
    (let* ((filter (params->filter))
           (cash-form (make-instance 'cash-form
                                     :direction direction
                                     :op :details
                                     :key (val tx-id)
                                     :cancel-url (apply #'cash direction :tx-id (val tx-id) filter)))
           (page-title (cash-page-title direction "Λεπτομέρειες")))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header)
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
    (("cash/" (direction "(incoming|outgoing)") "/create"))
    ((tx-date     date)
     (description string)
     (company     string  chk-company-title)
     (amount      float   chk-amount)
     (account-id  integer chk-account-id)
     (search      string)
     (since       date)
     (until       date))
  (with-view-page
    (check-cash-accounts)
    (let* ((filter (params->filter))
           (cash-form (make-instance 'cash-form
                                     :direction direction
                                     :op :create
                                     :cancel-url (apply #'cash direction filter)))
           (page-title (cash-page-title direction "Δημιουργία")))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header)
            (main-navbar 'cash)
            (cash-top-actions :create)
            (:div :class "grid_12"
              (:div :id "cash-window" :class "window"
                (:div :class "title" (str page-title))
                (actions cash-form :filter filter)
                (notifications)
                (with-form (actions/cash/create direction
                                                :search (val search)
                                                :since (val since)
                                                :until (val until))
                  (display cash-form :payload (params->payload)
                                     :styles (params->styles)))))
            (footer)))))))

(defpage cash-page actions/cash/create
    (("actions/cash/" (direction "(incoming|outgoing)") "/create") :request-type :post)
    ((tx-date     date)
     (description string)
     (company     string  chk-company-title t)
     (amount      float   chk-amount t)
     (account-id  integer chk-account-id t)
     (search      string)
     (since       date)
     (until       date))
  (with-controller-page (cash/create direction)
    (check-cash-accounts)
    (let* ((company-id (company-id (val company)))
           (debit-acc-id (cash-debit-acc-id direction (val account-id)))
           (credit-acc-id (cash-credit-acc-id direction (val account-id)))
           (new-tx (make-instance 'tx
                                  :tx-date (val tx-date)
                                  :description (val description)
                                  :company-id company-id
                                  :amount (val amount)
                                  :credit-acc-id credit-acc-id
                                  :debit-acc-id debit-acc-id
                                  :auto t)))
      (insert-dao new-tx)
      (see-other (apply #'cash/details direction :tx-id (tx-id new-tx)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage cash-page cash/update
    (("cash/" (direction "(incoming|outgoing)") "/update"))
    ((search      string)
     (since       date)
     (until       date)
     (tx-id       integer chk-tx-id         t)
     (tx-date     date)
     (description string)
     (company     string  chk-company-title)
     (amount      float   chk-amount)
     (account-id  integer chk-account-id))
  (with-view-page
    (check-cash-accounts)
    (let* ((filter (params->filter))
           (cash-form (make-instance 'cash-form
                                     :direction direction
                                     :op :update
                                     :key (val tx-id)
                                     :cancel-url (apply #'cash/details direction
                                                        :tx-id (val tx-id)
                                                        filter)))
           (page-title (cash-page-title direction "Επεξεργασία")))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header)
            (main-navbar 'cash)
            (cash-top-actions :update)
            (:div :class "grid_12"
              (:div :id "cash-window" :class "window"
                (:div :class "title" (str page-title))
                (actions cash-form :filter filter)
                (notifications)
                (with-form (actions/cash/update direction
                                                :tx-id (val tx-id)
                                                :search (val search)
                                                :since (val since)
                                                :until (val until))
                  (display cash-form :payload (params->payload)
                                     :styles (params->styles)))))
            (footer)))))))

(defpage cash-page actions/cash/update
    (("actions/cash/" (direction "(incoming|outgoing)") "/update") :request-type :post)
    ((search      string)
     (since       date)
     (until       date)
     (tx-id       integer chk-tx-id         t)
     (tx-date     date)
     (description string)
     (company     string  chk-company-title)
     (amount      float   chk-amount)
     (account-id  integer chk-account-id))
  (with-controller-page (cash/update direction)
    (check-cash-accounts)
    (let ((company-id (company-id (val company)))
          (debit-acc-id (cash-debit-acc-id direction (val account-id)))
          (credit-acc-id (cash-credit-acc-id direction (val account-id))))
      (execute (:update 'tx :set
                        'tx-date (val tx-date)
                        'description (val description)
                        'company-id company-id
                        'amount (val amount)
                        'debit-acc-id debit-acc-id
                        'credit-acc-id credit-acc-id
                        :where (:= 'id (val tx-id))))
      (see-other (apply #'cash/details direction :tx-id (val tx-id) (params->filter))))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage cash-page cash/delete
    (("cash/" (direction "(incoming|outgoing)") "/delete"))
    ((tx-id  integer chk-tx-id t)
     (search string)
     (since  date)
     (until  date))
  (with-view-page
    (check-cash-accounts)
    (let* ((filter (params->filter))
           (page-title (cash-page-title direction "Διαγραφή"))
           (cash-tx-table (make-instance 'cash-tx-table
                                         :op :delete
                                         :direction direction
                                         :selected-key (val tx-id)
                                         :filter filter)))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header)
            (main-navbar 'cash)
            (cash-top-actions :delete)
            (filters cash-tx-table)
            (:div :class "grid_12"
              (:div :id "cash-window" :class "window"
                (:div :class "title" (str page-title))
                (actions cash-tx-table)
                (with-form (actions/cash/delete direction
                                                :tx-id (val tx-id)
                                                :search (val search)
                                                :since (val since)
                                                :until (val until))
                  (display cash-tx-table))))
            (footer)))))))

(defpage cash-page actions/cash/delete
    (("actions/cash/" (direction "(incoming|outgoing)") "/delete") :request-type :post)
    ((tx-id  integer chk-tx-id t)
     (search string)
     (since  date)
     (until  date))
  (with-controller-page (cash/delete)
    (check-cash-accounts)
    (delete-dao (get-dao 'tx (val tx-id)))
    (see-other (apply #'cash direction (params->filter)))))
