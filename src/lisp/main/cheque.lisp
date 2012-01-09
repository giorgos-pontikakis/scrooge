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
    :initform '(bank due-date company amount tstamp serial))
   (filter-parameter-names
    :allocation :class
    :initform '(search since until cstate))
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

(defun chk-cheque-serial (serial)
  (declare (ignore serial))
  ;; do nothing for the time being
  nil)


;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun cheque-page-title (kind)
  (cond ((string-equal kind "receivable") "Εισπρακτέες")
        ((string-equal kind "payable") "Πληρωτέες")
        (t (error "Internal error in cheque-page-title"))))

(defun get-debit-credit-acc-id (kind)
  (cond ((string-equal kind "receivable")
         (values *cash-acc-id* *cheque-receivable-acc-id*))
        ((string-equal kind "payable")
         (values *cheque-payable-acc-id* *cash-acc-id*))
        (t
         (error "Internal error in get-debit-credit-acc-id"))))


(defun cheque-actions (op kind id filter)
  (actions-menu (make-menu-spec
                 (action-anchors/crud+details (apply #'cheque/create kind filter)
                                              (apply #'cheque/details kind :id id filter)
                                              (apply #'cheque/update kind :id id filter)
                                              (apply #'cheque/delete kind :id id filter)))
                (enabled-actions/crud+details op id)))

(defun cheque-filters (kind filter)
  (let ((filter* (remove-from-plist filter :cstate)))
    (with-html
      (:div :class "filters"
            (filters-navbar `((receivable ,(apply #'cheque "receivable" filter)
                                          "Προς είσπραξη")
                              (payable ,(apply #'cheque "payable" filter)
                                       "Προς πληρωμή"))
                            kind)
            (datebox (lambda (&rest args)
                       (apply #'cheque kind args))
                     filter)
            (filters-navbar `((nil      ,(apply #'cheque kind filter*)
                                        "Όλες")
                              (pending  ,(apply #'cheque kind :cstate "pending" filter*)
                                        "Σε εκκρεμότητα")
                              (paid     ,(apply #'cheque kind :cstate "paid" filter*)
                                        "Πληρωμένες")
                              (bounced  ,(apply #'cheque kind :cstate "bounced" filter*)
                                        "Ακάλυπτες")
                              (returned ,(apply #'cheque kind :cstate "returned" filter*)
                                        "Επιστραμμένες")
                              (stamped  ,(apply #'cheque kind :cstate "stamped" filter*)
                                        "Σφραγισμένες"))
                            (getf filter :cstate))))))

(defun cheque-subnavbar (op kind filter &optional id)
  (with-html
    (:div :class "section-subnavbar grid_12"
          (if (member op '(:catalogue :delete))
              (cheque-filters kind filter)
              (htm (:div :class "options"
                         (:ul (:li (:a :href (apply #'cheque kind :id id filter)
                                       "Κατάλογος"))))))
          (searchbox #'(lambda (&rest args)
                         (apply #'actions/cheque/search kind args))
                     #'(lambda (&rest args)
                         (apply #'cheque kind :id id args))
                     filter
                     "ac-company"))))

(defpage cheque-page actions/cheque/search
    (("actions/cheque/" (kind "(receivable|payable)") "/search") :request-type :get)
    ((search string))
  (with-db ()
    (let* ((filter (params->filter))
           (records (get-records (make-instance 'cheque-table
                                                :kind kind
                                                :filter filter))))
      (if (or (not records)
              (and records (cdr records)))
          (see-other (apply #'cheque kind filter))
          (see-other (apply #'cheque/details kind :id (getf (first records) :id) filter))))))



;;; ----------------------------------------------------------------------
;;; Cheque form
;;; ----------------------------------------------------------------------

(defclass cheque-form (crud-form/plist)
  ((kind :accessor kind :initarg :kind)))

(defmethod display ((form cheque-form) &key styles)
  (let* ((record (record form))
         (disabled (eql (op form) :details))
         (lit (label-input-text disabled record styles))
         (events (get-cheque-events (getf record :id)))
         (following (following-cheque-states (getf record :state-id)
                                             (getf record :payable-p)))
         (tstamp-format '((:day 2) #\- (:month 2) #\- (:year 4) " --- " (:hour 2) ":" (:min 2))))
    (let ((*default-timezone* +greek-zone+))
      (with-html
        (:div :id "cheque-data-form" :class "data-form"
              (:div :class "grid_6 alpha"
                    (display lit 'serial "Σειριακός Αριθμός")
                    (display lit 'due-date "Ημερομηνία" "datepicker")
                    (display lit 'company "Εταιρία" "ac-company")
                    (display lit 'bank "Τράπεζα" "ac-bank")
                    (display lit 'amount "Ποσό"))
              (:div :class "grid_6 omega"
                    (:table :class "crud-table"
                            (:thead (:tr (:th "Κατάσταση") (:th "Χρονικό σημείο αλλαγής")))
                            (iter
                              (for ev in events)
                              (htm (:tr
                                    (:td (str (assoc-value *cheque-states*
                                                           (getf ev :to-state-id)
                                                           :test #'string=)))
                                    (:td (str (format-timestring nil (getf ev :tstamp)
                                                                 :format tstamp-format)))))))
                    (when (and following (not disabled))
                      (htm (:p "Αλλαγή κατάστασης: " (dropdown 'state-id
                                                               (acons "nil" "" following))))))
              (:div :class "grid_12 data-form-buttons"
                    (unless disabled
                      (ok-button :body (if (eql (op form) :update) "Ανανέωση" "Δημιουργία"))
                      (cancel-button (cancel-url form) :body "Άκυρο"))))))))

(defun following-cheque-states (from-state-id payable-p)
  (lists->alist
   (query (:select 'cheque-stran.to-state-id 'cheque-state.description
                   :from 'cheque-stran
                   :inner-join 'cheque-state
                   :on (:= 'cheque-stran.to-state-id 'cheque-state.id)
                   :where (:and (:= 'from-state-id from-state-id)
                                (:= 'payable-p payable-p))))))



;;; ----------------------------------------------------------------------
;;; Database interface
;;; ----------------------------------------------------------------------

(defmethod get-record ((type (eql 'cheque)) id)
  (declare (ignore type))
  (query (:select 'cheque.id (:as 'bank.title 'bank)
                  'due-date (:as 'company.title 'company)
                  'amount 'payable-p 'state-id 'serial
                  :from 'cheque
                  :left-join 'bank
                  :on (:= 'bank.id 'cheque.bank-id)
                  :inner-join 'company
                  :on (:= 'company.id 'cheque.company-id)
                  :where (:= 'cheque.id id))
         :plist))

(defun get-cheque-events (cheque-id)
  (query (:order-by (:select 'to-state-id 'tstamp
                             :from 'cheque-event
                             :where (:= 'cheque-id cheque-id))
                    'tstamp)
         :plists))



;;; ----------------------------------------------------------------------
;;; Cheques table
;;; ----------------------------------------------------------------------

;;; table

(defclass cheque-table (scrooge-table)
  ((kind        :accessor kind :initarg :kind)
   (header-labels  :initform '("" "Σειριακός<br />Αριθμός" "<br />Εταιρία" "<br />Τράπεζα"
                               "Ημερομηνία<br />πληρωμής" "<br />Ποσό"))
   (paginator      :initform (make-instance 'cheque-paginator
                                            :id "cheque-paginator"
                                            :css-class "paginator")))
  (:default-initargs :item-class 'cheque-row))

(defmethod get-records ((table cheque-table))
  (let* ((search (getf (filter table) :search))
         (since (getf (filter table) :since))
         (until (getf (filter table) :until))
         (cstate (getf (filter table) :cstate))
         (payable-p (string= (kind table) "payable"))
         (base-query `(:select cheque.id (:as bank.title bank) serial
                               due-date (:as company.title company) amount payable-p
                               :from cheque
                               :left-join bank
                               :on (:= bank.id cheque.bank-id)
                               :inner-join company
                               :on (:= company.id cheque.company-id)))
         (where nil))
    (when search
      (push `(:or (:ilike company.title ,(ilike search))
                  (:ilike bank.title ,(ilike search)))
            where))
    (when cstate
      (push `(:= ,cstate state-id)
            where))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since due-date) where))
    (when (and until (not (eql until :null)))
      (push `(:<= due-date ,until) where))
    (let ((sql `(:order-by (,@base-query :where
                                         (:and (:= cheque.payable-p ,payable-p)
                                               ,@where))
                           due-date)))
      (query (sql-compile sql)
             :plists))))


;;; rows

(defclass cheque-row (scrooge-row/plist)
  ())

(defmethod selector ((row cheque-row) selected-p)
  (let* ((id (key row))
         (table (collection row))
         (pg (paginator table))
         (filter (filter table))
         (kind (kind table))
         (start (start-index table)))
    (html ()
      (:a :href (if selected-p
                    (apply #'cheque kind
                           :start (page-start pg (index row) start)
                           filter)
                    (apply #'cheque kind :id id filter))
          (selector-img selected-p)))))

(defmethod payload ((row cheque-row) enabled-p)
  (let ((record (record row)))
    (list (make-instance 'textbox
                         :name 'serial
                         :value (getf record :serial)
                         :disabled (not enabled-p))
          (make-instance 'textbox
                         :name 'company
                         :value (getf record :company)
                         :css-class "ac-company"
                         :disabled (not enabled-p))
          (make-instance 'textbox
                         :name 'bank
                         :value (getf record :bank)
                         :css-class "ac-bank"
                         :disabled (not enabled-p))
          (make-instance 'textbox
                         :name 'due-date
                         :value (getf record :due-date)
                         :css-class (if enabled-p "datepicker" nil)
                         :disabled (not enabled-p))
          (make-instance 'textbox
                         :name 'amount
                         :value (getf record :amount)
                         :disabled (not enabled-p)))))

(defmethod controls ((row cheque-row) controls-p)
  (let* ((id (key row))
         (table (collection row))
         (filter (filter table))
         (kind (kind table)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (apply #'cheque kind :id id filter)))
        (list nil nil))))


;;; paginator

(defclass cheque-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg cheque-paginator) start)
  (let ((table (table pg)))
   (apply #'cheque (kind table) :start start (filter table))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage cheque-page cheque (("cheque/" (kind "(receivable|payable)")))
    ((search string)
     (cstate string  chk-cheque-state-id)
     (since  date    chk-date)
     (until  date    chk-date)
     (start  integer)
     (id     integer chk-cheque-id))
  (with-view-page
    (check-cheque-accounts)
    (let* ((op :catalogue)
           (filter (params->filter))
           (page-title (conc "Επιταγές » " (cheque-page-title kind) " » Κατάλογος"))
           (cheque-table (make-instance 'cheque-table
                                        :id "cheque-table"
                                        :kind kind
                                        :op op
                                        :filter filter
                                        :start-index (val start))))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cheque)
               (cheque-subnavbar op kind filter (val id))
               (:div :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cheque-actions op kind (val id) filter)
                     (display cheque-table
                              :key (val id)
                              :payload nil))
               (footer)))))))

(defpage cheque-page cheque/details (("cheque/" (kind "(receivable|payable)") "/details"))
    ((search string)
     (cstate string  chk-cheque-state-id)
     (since  date    chk-date)
     (until  date    chk-date)
     (start  integer)
     (id     integer chk-cheque-id       t))
  (with-view-page
    (check-cheque-accounts)
    (let* ((op :details)
           (filter (params->filter))
           (page-title (conc "Επιταγές » " (cheque-page-title kind) " » Λεπτομέρειες"))
           (cheque-form (make-instance 'cheque-form
                                       :kind kind
                                       :op op
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
               (cheque-subnavbar op kind filter (val id))
               (:div :id "cheque-window" :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cheque-actions op kind (val id) filter)
                     (display cheque-form))
               (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/create (("cheque/" (kind  "(receivable|payable)") "/create"))
    ((search   string)
     (cstate   string  chk-cheque-state-id)
     (since    date    chk-date)
     (until    date    chk-date)
     (serial   string  chk-cheque-serial)
     (bank     string  chk-bank-title)
     (due-date date    chk-date)
     (company  string  chk-company-title)
     (amount   float   chk-amount))
  (with-view-page
    (check-cheque-accounts)
    (let* ((op :create)
           (filter (params->filter))
           (page-title (conc "Επιταγές » " (cheque-page-title kind) " » Δημιουργία"))
           (cheque-table (make-instance 'cheque-table
                                        :kind kind
                                        :op :create
                                        :filter filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cheque)
               (cheque-subnavbar op kind filter)
               (:div :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cheque-actions op kind nil filter)
                     (notifications))
               (with-form (actions/cheque/create kind
                                                 :search (val search)
                                                 :since (val since)
                                                 :until (val until)
                                                 :cstate (val cstate))
                 (display cheque-table :payload (params->payload)))
               (footer)))))))

(defpage cheque-page actions/cheque/create
    (("actions/cheque/" (kind "(receivable|payable)") "/create") :request-type :post)
    ((search   string)
     (cstate   string  chk-cheque-state-id)
     (since    date    chk-date)
     (until    date    chk-date)
     (bank     string  chk-bank-title)
     (serial   string  chk-cheque-serial)
     (company  string  chk-company-title t)
     (due-date date    chk-date          t)
     (amount   float   chk-amount        t))
  (with-controller-page (cheque/create kind)
    (check-cheque-accounts)
    (let* ((bank-id (bank-id (val bank)))
           (company-id (company-id (val company)))
           (cheque-stran (select-dao-unique 'cheque-stran
                             (:= 'from-state-id "nil")))
           (temtx (select-dao-unique 'temtx
                      (:= 'id (temtx-id cheque-stran))))
           (new-tx (make-instance 'tx
                                  :tx-date (today)
                                  :description (title temtx)
                                  :company-id company-id
                                  :amount (val amount)
                                  :credit-acc-id (credit-acc-id temtx)
                                  :debit-acc-id (debit-acc-id temtx)))
           (new-cheque (make-instance 'cheque
                                      :serial (val serial)
                                      :bank-id bank-id
                                      :company-id company-id
                                      :due-date (val due-date)
                                      :amount (val amount)
                                      :payable-p (string= kind "payable")
                                      :state-id "pending")))
      (with-transaction ()
        (insert-dao new-cheque)
        (insert-dao new-tx)
        (insert-dao (make-instance 'cheque-event
                                   :tstamp (now)
                                   :cheque-id (cheque-id new-cheque)
                                   :from-state-id (from-state-id cheque-stran)
                                   :to-state-id (to-state-id cheque-stran)
                                   :tx-id (tx-id new-tx))))
      (see-other (apply #'cheque kind :id (cheque-id new-cheque) (params->filter))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/update (("cheque/" (kind "(receivable|payable)") "/update"))
    ((search   string)
     (cstate   string  chk-cheque-state-id)
     (since    date    chk-date)
     (until    date    chk-date)
     (id       integer chk-cheque-id     t)
     (serial   string  chk-cheque-serial)
     (bank     string  chk-bank-title)
     (company  string  chk-company-title)
     (due-date date    chk-date)
     (amount   float   chk-amount)
     (state-id string  chk-cheque-state-id))
  (with-view-page
    (check-cheque-accounts)
    (let* ((op :update)
           (page-title (conc "Επιταγές » " (cheque-page-title kind) " » Επεξεργασία"))
           (filter (params->filter))
           (cheque-form (make-instance 'cheque-form
                                       :kind kind
                                       :op op
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
               (cheque-subnavbar op kind filter (val id))
               (:div :id "cheque-window" :class "window grid_12"
                     (:p :class "title" (str page-title))
                     (cheque-actions op kind (val id) filter)
                     (notifications)
                     (with-form (actions/cheque/update kind
                                                       :id (val id)
                                                       :search (val search)
                                                       :since (val since)
                                                       :until (val until)
                                                       :cstate (val cstate))
                       (display cheque-form :key (val id)
                                            :payload (params->payload))))
               (footer)))))))

(defpage cheque-page actions/cheque/update
    (("actions/cheque/" (kind "(receivable|payable)") "/update") :request-type :post)
    ((search   string)
     (cstate   string  chk-cheque-state-id)
     (since    date    chk-date)
     (until    date    chk-date)
     (id       integer chk-cheque-id     t)
     (serial   string  chk-cheque-serial)
     (bank     string  chk-bank-title)
     (company  string  chk-company-title t)
     (due-date date    chk-date          t)
     (amount   float   chk-amount        t)
     (state-id string  chk-cheque-state-id))
  (with-controller-page (cheque/update kind)
    (check-cheque-accounts)
    (let* ((bank-id (bank-id (val bank)))
           (company-id (company-id (val company)))
           (cheque-dao (get-dao 'cheque (val id))))
      (let ((from-state-id (state-id cheque-dao)))
        (with-transaction ()
          ;; When a new state is requested, update the state of cheque-dao and create a new
          ;; event and the corresponding tx
          (unless (string= (val state-id) "nil")
            (let* ((cheque-stran (select-dao-unique 'cheque-stran
                                     (:and (:= 'from-state-id from-state-id)
                                           (:= 'to-state-id (val state-id)))))
                   (temtx (select-dao-unique 'temtx
                              (:= 'id (temtx-id cheque-stran))))
                   (new-tx (make-instance 'tx
                                          :tx-date (today)
                                          :description (title temtx)
                                          :company-id company-id
                                          :amount (val amount)
                                          :credit-acc-id (credit-acc-id temtx)
                                          :debit-acc-id (debit-acc-id temtx))))

              (insert-dao new-tx)
              (insert-dao (make-instance 'cheque-event
                                         :tstamp (now)
                                         :cheque-id (cheque-id cheque-dao)
                                         :from-state-id (from-state-id cheque-stran)
                                         :to-state-id (to-state-id cheque-stran)
                                         :tx-id (tx-id new-tx)))
              (setf (state-id cheque-dao) (val state-id))))
          ;; In any case, update cheque's data
          (setf (serial cheque-dao) (val serial)
                (bank-id cheque-dao) bank-id
                (company-id cheque-dao) company-id
                (due-date cheque-dao) (val due-date)
                (amount cheque-dao) (val amount))
          (update-dao cheque-dao))
        ;; Finally redirect
        (see-other (apply #'cheque/details kind :id (val id) (params->filter)))))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/delete (("cheque/" (kind "(receivable|payable)") "/delete"))
    ((search string)
     (cstate string  chk-cheque-state-id)
     (since  date    chk-date)
     (until  date    chk-date)
     (id     integer chk-cheque-id t))
  (with-view-page
    (check-cheque-accounts)
    (let* ((op :delete)
           (page-title (conc "Επιταγές » " (cheque-page-title kind) " » Διαγραφή"))
           (filter (params->filter))
           (cheque-table (make-instance 'cheque-table
                                        :kind kind
                                        :op :delete
                                        :filter filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cheque)
               (cheque-subnavbar op kind filter (val id))
               (:div :class "window"
                     (:div :class "window grid_12"
                           (:div :class "title" (str page-title))
                           (cheque-actions op kind (val id) filter)
                           (with-form (actions/cheque/delete kind
                                                             :id (val id)
                                                             :search (val search)
                                                             :since (val since)
                                                             :until (val until)
                                                             :cstate (val cstate))
                             (display cheque-table :key (val id)))))
               (footer)))))))

(defpage cheque-page actions/cheque/delete
    (("actions/cheque/" (kind "(receivable|payable)") "/delete") :request-type :post)
    ((search string)
     (cstate string  chk-cheque-state-id)
     (since  date    chk-date)
     (until  date    chk-date)
     (id     integer chk-cheque-id t))
  (with-controller-page ()
    (check-cheque-accounts)
    (with-transaction ()
      (let* ((cheque-dao (get-dao 'cheque (val id)))
             (cheque-event-daos (select-dao 'cheque-event (:= 'cheque-id (cheque-id cheque-dao))))
             (tx-daos (mapcar (compose (lambda (tx-id) (get-dao 'tx tx-id))
                                       #'tx-id)
                              cheque-event-daos)))
        (mapc #'delete-dao cheque-event-daos)
        (mapc #'delete-dao tx-daos)
        (delete-dao cheque-dao)))
    (see-other (apply #'cheque kind (params->filter)))))
