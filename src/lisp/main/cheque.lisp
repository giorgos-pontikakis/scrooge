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
    :initform '(bank due-date company amount state))
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
  (actions-menu (crud-actions-spec (apply #'cheque/create kind filter)
                                           (apply #'cheque/update kind :id id filter)
                                           (apply #'cheque/delete kind :id id filter))
                (crud-actions-enabled/disabled op id)))

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
                                        "Επιστραμμένες"))
                            (getf filter :cstate))))))

(defun cheque-subnavbar (kind filter)
  (with-html
    (:div :class "section-subnavbar grid_12"
          (cheque-filters kind filter)
          (searchbox #'(lambda (&rest args)
                         (apply #'cheque kind args))
                     filter
                     "ac-company"))))



;;; ----------------------------------------------------------------------
;;; Database interface
;;; ----------------------------------------------------------------------

(defmethod get-record ((type (eql 'cheque)) id)
  (declare (ignore type))
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

(defmethod get-records ((table cheque-table))
  (let* ((search (getf (filter table) :search))
         (since (getf (filter table) :since))
         (until (getf (filter table) :until))
         (cstate (getf (filter table) :cstate))
         (payable-p (string= (kind table) "payable"))
         (base-query `(:select cheque.id (:as bank.title bank)
                               due-date (:as company.title company) amount payable-p
                               :from cheque
                               :inner-join bank
                               :on (:= bank.id cheque.bank-id)
                               :inner-join company
                               :on (:= company.id cheque.company-id)
                               :inner-join cheque-event
                               :on (:= cheque-event.cheque-id cheque.id)))
         (where nil))
    (when search
      (push `(:or (:ilike company.title ,(ilike search))
                  (:ilike bank.title ,(ilike search)))
            where))
    (when cstate
      (push `(:= cheque.state ,cstate)
            where))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since due-date) where))
    (when (and until (not (eql until :null)))
      (push `(:<= due-date ,until) where))
    (let ((sql `(:order-by (,@base-query :where
                                         (:and (:= cheque.payable-p ,payable-p)
                                               ,@where))
                           (:desc due-date))))
      (with-db ()
        (query (sql-compile sql)
               :plists)))))


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
                                        :filter filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cheque)
               (cheque-subnavbar kind filter)
               (:div :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cheque-actions op kind (val id) filter)
                     (display cheque-table
                              :key (val id)
                              :payload nil
                              :start (val start)))
               (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/create (("cheque/" (kind  "(receivable|payable)") "/create"))
    ((search     string)
     (cstate     string  chk-cheque-state-id)
     (since      date    chk-date)
     (until      date    chk-date)
     (bank       string  chk-bank-title)
     (due-date   date    chk-date)
     (company    string  chk-company-title)
     (amount     float   chk-amount))
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
               (cheque-subnavbar kind filter)
               (:div :class "window grid_12"
                     (:div :class "title" (str page-title))
                     (cheque-actions op kind nil filter))
               (with-form (actions/cheque/create kind
                                                 :search (val search)
                                                 :since (val since)
                                                 :until (val until)
                                                 :cstate (val cstate))
                 (display cheque-table :payload (params->payload)))
               (footer)))))))

(defpage cheque-page actions/cheque/create
    (("actions/cheque/" (kind "(receivable|payable)") "/create") :request-type :post)
    ((search     string)
     (cstate     string  chk-cheque-state-id)
     (since      date    chk-date)
     (until      date    chk-date)
     (bank       string  chk-bank-title)
     (company    string  chk-company-title t)
     (due-date   date    chk-date          t)
     (amount     float   chk-amount        t))
  (with-controller-page (cheque/create)
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
                                       :bank-id bank-id
                                       :company-id company-id
                                       :due-date (val due-date)
                                       :amount (val amount)
                                       :payable-p (string= kind "payable"))))
      (with-transaction ()
        (insert-dao new-cheque)
        (insert-dao new-tx)
        (insert-dao (make-instance 'cheque-event
                                   :tstamp (now)
                                   :cheque-id (cheque-id new-cheque)
                                   :from-state-id (from-state-id cheque-stran)
                                   :to-state-id (to-state-id cheque-stran)
                                   :tx-id (tx-id new-tx))))
      (see-other (apply #'cheque/create kind :id (cheque-id new-cheque) (params->filter))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/update (("cheque/" (kind "(receivable|payable)") "/update"))
    ((search     string)
     (cstate     string  chk-cheque-state-id)
     (since      date    chk-date)
     (until      date    chk-date)
     (id         integer chk-cheque-id     t)
     (bank       string  chk-bank-title)
     (company    string  chk-company-title)
     (due-date   date    chk-date)
     (state      string  chk-cheque-state-id)
     (amount     float   chk-amount))
  (with-view-page
    (check-cheque-accounts)
    (let* ((op :update)
           (page-title (conc "Επιταγές » " (cheque-page-title kind) " » Επεξεργασία"))
           (filter (params->filter))
           (cheque-table (make-instance 'cheque-table
                                        :kind kind
                                        :op :update
                                        :filter filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cheque)
               (cheque-subnavbar kind filter)
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
                       (display cheque-table :key (val id)
                                             :payload (params->payload))))
               (footer)))))))

(defpage cheque-page actions/cheque/update
    (("actions/cheque/" (kind "(receivable|payable)") "/update") :request-type :post)
    ((search   string)
     (cstate   string  chk-cheque-state-id)
     (since    date    chk-date)
     (until    date    chk-date)
     (id       integer chk-cheque-id     t)
     (bank     string  chk-bank-title)
     (company  string  chk-company-title t)
     (due-date date    chk-date          t)
     (amount   float   chk-amount        t)
     (state    string  chk-cheque-state-id  t))
  (with-controller-page (cheque/update kind)
    (check-cheque-accounts)
    (let* ((bank-id (bank-id (val bank)))
           (company-id (company-id (val company)))
           (cheque-dao (get-dao 'cheque (val id)))
           (cheque-stran (select-dao-unique 'cheque-stran
                                            (:and (:= 'from-state (state cheque-dao))
                                                  (:= 'to-state (val state)))))
           (temtx (select-dao 'temtx
                              (:= 'id (temtx-id cheque-stran))))
           (new-tx (if (string= (val state) (state cheque-dao))
                       nil
                       (make-instance 'tx
                                      :tx-date (today)
                                      :description (title temtx)
                                      :company-id company-id
                                      :amount (val amount)
                                      :credit-acc-id (credit-acc-id temtx)
                                      :debit-acc-id (debit-acc-id temtx)
                                      :auto t))))
      (with-transaction ()
        (setf (bank-id cheque-dao) bank-id
              (company-id cheque-dao) company-id
              (due-date cheque-dao) (val due-date)
              (amount cheque-dao) (val amount)
              (state cheque-dao) (val state))
        (update-dao cheque-dao)
        (insert-dao new-tx)
        (insert-dao (make-instance 'cheque-event
                                   :event-date (today)
                                   :cheque-id (id cheque-dao)
                                   :cheque-stran-id (id cheque-stran)
                                   :tx-id (id new-tx)))))))



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
               (cheque-subnavbar kind filter)
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
             (cheque-event-daos (select-dao 'cheque-event (:= 'cheque-id (id cheque-dao))))
             (tx-daos (mapcar #'tx-id cheque-event-daos)))
        (mapc #'delete-dao tx-daos)
        (mapc #'delete-dao cheque-event-daos)
        (delete-dao cheque-dao)))
    (see-other (apply #'cheque kind (params->filter)))))
