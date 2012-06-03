(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass cheque-family (family-mixin)
  ()
  (:default-initargs
   :parameter-groups '(:system (cheque-id start)
                       :payload (bank due-date company amount serial state-id tstamp)
                       :filter (search cstate since until))))



(defclass cheque-page (auth-regex-page cheque-family)
  ((messages
    :allocation :class
    :reader messages
    :initform '((due-date (:date-null
                           "Η ημερομηνία της επιταγής είναι κενή"
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
                  "Το ποσό της επιταγής είναι κενό"
                  :non-positive-amount
                  "Το ποσό της επιταγής δεν είναι θετικός αριθμός"
                  :amount-overflow
                  "Το ποσό της επιταγής είναι δεν πρέπει να ξεπερνά το 9,999,999.99"
                  :parse-error
                  "Το ποσό της επιταγής περιέχει άκυρους χαρακτήρες"))))))



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

(defun chk-cheque-id (cheque-id)
  (if (cheque-id-exists-p cheque-id)
      nil
      :cheque-id-unknown))

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

(defun cheque-top-actions (op)
  (let ((kind (first *registers*)))
    (top-actions (make-instance 'scrooge-menu
                                :spec (make-menu-spec
                                       (list :catalogue (family-url 'cheque :system :filter)
                                             :create (list (family-url 'cheque/create :filter)
                                                           (conc "Νέα "
                                                                 (if (string-equal kind "receivable")
                                                                     "Εισπρακτέα"
                                                                     "Πληρωτέα")
                                                                 " επιταγή"))))
                                :css-class "hmenu"
                                :disabled (list op))
                 (searchbox (family-url-fn 'actions/cheque/search)
                            (family-url-fn 'cheque :system)
                            (family-params 'cheque :filter)
                            "ac-company"))))



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
              (:div :class "grid_5 prefix_1 alpha"
                    (display lit 'serial "Σειριακός Αριθμός")
                    (display lit 'due-date "Ημερομηνία" :extra-styles "datepicker")
                    (display lit 'company "Εταιρία" :extra-styles "ac-company")
                    (display lit 'bank "Τράπεζα" :extra-styles "ac-bank")
                    (display lit 'amount "Ποσό")
                    (:div :class "data-form-buttons"
                          (unless disabled
                            (ok-button :body (if (eql (op form) :update) "Ανανέωση" "Δημιουργία"))
                            (cancel-button (cancel-url form) :body "Άκυρο"))))
              (:div :class "prefix_1 grid_4 omega"
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
              (clear))))))

(defun following-cheque-states (from-state-id payable-p)
  (lists->alist
   (query (:select 'cheque-stran.to-state-id 'cheque-state.description
                   :from 'cheque-stran
                   :inner-join 'cheque-state
                   :on (:= 'cheque-stran.to-state-id 'cheque-state.id)
                   :where (:and (:= 'from-state-id from-state-id)
                                (:= 'payable-p payable-p))))))

(defmethod get-record ((form cheque-form))
  (if-let (cheque-id (key form))
    (query (:select 'cheque.id (:as 'bank.title 'bank)
                    'due-date (:as 'company.title 'company)
                    'amount 'payable-p 'state-id 'serial
                    :from 'cheque
                    :left-join 'bank
                    :on (:= 'bank.id 'cheque.bank-id)
                    :inner-join 'company
                    :on (:= 'company.id 'cheque.company-id)
                    :where (:= 'cheque.id cheque-id))
           :plist)
    nil))

(defmethod actions ((form cheque-form) &key filter)
  (let* ((cheque-id (key form))
         (kind (kind form))
         (hrefs (list :update (apply #'cheque/update kind :cheque-id cheque-id filter)
                      :delete (apply #'cheque/delete kind :cheque-id cheque-id filter))))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions form))))



;;; ----------------------------------------------------------------------
;;; Database interface
;;; ----------------------------------------------------------------------

(defun get-cheque-events (cheque-id)
  (query (:order-by (:select 'to-state-id 'tstamp
                             :from 'cheque-event
                             :where (:= 'cheque-id cheque-id))
                    'tstamp)
         :plists))

(defun get-cheque-records (table &optional company-id)
  (let* ((search (getf (filter table) :search))
         (since (getf (filter table) :since))
         (until (getf (filter table) :until))
         (cstate (getf (filter table) :cstate))
         (payable-p (string= (kind table) "payable"))
         (base-query `(:select cheque.id (:as bank.title bank) serial state-id
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
      (push `(:= ,cstate state-id) where))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since due-date) where))
    (when (and until (not (eql until :null)))
      (push `(:<= due-date ,until) where))
    (when company-id
      (push `(:= company.id ,company-id) where))
    (when (kind table)
      (push `(:= cheque.payable-p ,payable-p) where))
    (let ((sql `(:order-by (,@base-query :where
                                         (:and
                                          ,@where))
                           due-date)))
      (query (sql-compile sql)
             :plists))))



;;; ----------------------------------------------------------------------
;;; Cheque table
;;; ----------------------------------------------------------------------

;;; table

(defclass cheque-table (scrooge-table)
  ((kind      :accessor kind      :initarg :kind)
   (paginator :accessor paginator :initarg :paginator)
   (header-labels :initform '("" "Σειριακός<br />Αριθμός" "<br />Εταιρία" "<br />Τράπεζα"
                               "Ημερομηνία<br />λήξης" "<br />Ποσό")))
  (:default-initargs :item-class 'cheque-row
                     :id "cheque-table"
                     :kind nil
                     :paginator (make-instance 'cheque-paginator
                                              :id "cheque-paginator"
                                              :css-class "paginator")))

(defmethod get-records ((table cheque-table))
  (get-cheque-records table))

(defmethod actions ((tbl cheque-table) &key)
  (let* ((cheque-id (selected-key tbl))
         (kind (kind tbl))
         (filter (filter tbl))
         (hrefs (if cheque-id
                    (list :details (apply #'cheque/details kind :cheque-id cheque-id filter)
                          :update (apply #'cheque/update kind :cheque-id cheque-id filter)
                          :delete (apply #'cheque/delete kind :cheque-id cheque-id filter))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))

(defmethod filters ((tbl cheque-table))
  (let* ((kind (kind tbl))
         (filter (filter tbl))
         (filter* (remove-from-plist filter kind :cstate))
         (filter-spec `((nil      ,(apply #'cheque kind filter*)
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
                                  "Σφραγισμένες"))))
    (filter-area (filter-navbar `((receivable ,(apply #'cheque "receivable" filter)
                                              "Προς είσπραξη")
                                  (payable ,(apply #'cheque "payable" filter)
                                           "Προς πληρωμή"))
                                :active kind
                                :id "cheque-kind-navbar")
                 (filter-navbar filter-spec
                                :active (getf filter :cstate))
                 (datebox (lambda (&rest args)
                            (apply #'cheque kind args))
                          filter))))


;;; rows

(defclass cheque-row (scrooge-row/plist)
  ())

(defmethod selector ((row cheque-row) selected-p)
  (let* ((cheque-id (key row))
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
                    (apply #'cheque kind :cheque-id cheque-id filter))
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
                         :value (fmt-amount (getf record :amount))
                         :disabled (not enabled-p)))))

(defmethod controls ((row cheque-row) controls-p)
  (let* ((cheque-id (key row))
         (table (collection row))
         (filter (filter table))
         (kind (kind table)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (apply #'cheque kind :cheque-id cheque-id filter)))
        (list nil nil))))


;;; paginator

(defclass cheque-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg cheque-paginator) start)
  (let ((table (table pg)))
   (apply #'cheque (kind table) :start start (filter table))))



;;; ------------------------------------------------------------
;;; SEARCH
;;; ------------------------------------------------------------

(defpage cheque-page actions/cheque/search
    (("actions/cheque/" (kind "(receivable|payable)") "/search") :request-type :get)
    ((search string)
     (cstate    string  chk-cheque-state-id)
     (since     date    chk-date)
     (until     date    chk-date))
  (with-db ()
    (let* ((filter (params->filter))
           (rows (rows (make-instance 'cheque-table
                                      :kind kind
                                      :filter filter))))
      (if (single-item-list-p rows)
          (see-other (apply #'cheque/details kind
                            :cheque-id (key (first rows))
                            filter))
          (see-other (apply #'cheque kind filter))))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage cheque-page cheque (("cheque/" (kind "(receivable|payable)")))
    ((cheque-id integer chk-cheque-id)
     (start     integer)
     (search    string)
     (cstate    string  chk-cheque-state-id)
     (since     date    chk-date)
     (until     date    chk-date))
  (with-view-page
    (check-cheque-accounts)
    (let* ((filter (params->filter))
           (page-title (conc "Επιταγές » " (cheque-page-title kind) " » Κατάλογος"))
           (cheque-table (make-instance 'cheque-table
                                        :id "cheque-table"
                                        :kind kind
                                        :op :catalogue
                                        :selected-key (val cheque-id)
                                        :filter filter
                                        :start-index (val start))))
      ;; if cheque-id exists and is not found among records, ignore search term
      (when (and (val cheque-id)
                 (not (find (val cheque-id) (rows cheque-table) :key #'key)))
        (let ((dao (get-dao 'cheque (val cheque-id))))
          (see-other (cheque (if (payable-p dao) "payable" "receivable")
                             :cheque-id (val cheque-id)
                             :cstate (state-id dao)))))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cheque)
               (cheque-top-actions :catalogue)
               (filters cheque-table)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" (str page-title))
                           (actions cheque-table)
                           (display cheque-table)))
               (footer)))))))

(defpage cheque-page cheque/details (("cheque/" (kind "(receivable|payable)") "/details"))
    ((cheque-id integer chk-cheque-id       t)
     (search    string)
     (cstate    string  chk-cheque-state-id)
     (since     date    chk-date)
     (until     date    chk-date))
  (with-view-page
    (check-cheque-accounts)
    (let* ((filter (params->filter))
           (page-title (conc "Επιταγές » " (cheque-page-title kind) " » Λεπτομέρειες"))
           (cheque-form (make-instance 'cheque-form
                                       :kind kind
                                       :op :details
                                       :key (val cheque-id)
                                       :cancel-url (apply #'cheque
                                                          kind
                                                          :cheque-id (val cheque-id)
                                                          filter))))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cheque)
               (cheque-top-actions :details)
               (:div :class "grid_12"
                     (:div :id "cheque-window" :class "window"
                           (:div :class "title" (str page-title))
                           (actions cheque-form :filter filter)
                           (display cheque-form)))
               (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/create (("cheque/" (kind  "(receivable|payable)") "/create"))
    ((bank     string  chk-bank-title)
     (due-date date    chk-date)
     (company  string  chk-company-title)
     (amount   float   chk-amount)
     (serial   string  chk-cheque-serial)
     (search   string)
     (cstate   string  chk-cheque-state-id)
     (since    date    chk-date)
     (until    date    chk-date))
  (with-view-page
    (check-cheque-accounts)
    (let* ((filter (params->filter))
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
               (cheque-top-actions :create)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" (str page-title))
                           (actions cheque-table)
                           (notifications)
                           (with-form (actions/cheque/create kind
                                                             :search (val search)
                                                             :since (val since)
                                                             :until (val until)
                                                             :cstate (val cstate))
                             (display cheque-table :payload (params->payload)))))
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
    (let ((new-cheque (make-instance 'cheque
                                     :serial (val serial)
                                     :bank-id (bank-id (val bank))
                                     :company-id (company-id (val company))
                                     :due-date (val due-date)
                                     :amount (val amount)
                                     :payable-p (string= kind "payable")
                                     :state-id "pending")))
      (insert-dao new-cheque)
      (see-other (apply #'cheque kind :cheque-id (cheque-id new-cheque) (params->filter))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/update (("cheque/" (kind "(receivable|payable)") "/update"))
    ((cheque-id integer chk-cheque-id       t)
     (bank      string  chk-bank-title)
     (company   string  chk-company-title)
     (due-date  date    chk-date)
     (amount    float   chk-amount)
     (serial    string  chk-cheque-serial)
     (state-id  string  chk-cheque-state-id)
     (search    string)
     (cstate    string  chk-cheque-state-id)
     (since     date    chk-date)
     (until     date    chk-date))
  (with-view-page
    (check-cheque-accounts)
    (let* ((filter (params->filter))
           (cheque-form (make-instance 'cheque-form
                                       :kind kind
                                       :op :update
                                       :key (val cheque-id)
                                       :cancel-url (apply #'cheque/details
                                                          kind
                                                          :cheque-id (val cheque-id)
                                                          filter)))
           (page-title (conc "Επιταγές » " (cheque-page-title kind) " » Επεξεργασία")))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cheque)
               (cheque-top-actions :update)
               (:div :class "grid_12"
                     (:div :id "cheque-window" :class "window"
                           (:p :class "title" (str page-title))
                           (actions cheque-form :filter filter)
                           (notifications)
                           (with-form (actions/cheque/update kind
                                                             :cheque-id (val cheque-id)
                                                             :search (val search)
                                                             :since (val since)
                                                             :until (val until)
                                                             :cstate (val cstate))
                             (display cheque-form :payload (params->payload)))))
               (footer)))))))

(defpage cheque-page actions/cheque/update
    (("actions/cheque/" (kind "(receivable|payable)") "/update") :request-type :post)
    ((cheque-id integer chk-cheque-id       t)
     (bank      string  chk-bank-title)
     (due-date  date    chk-date            t)
     (company   string  chk-company-title   t)
     (amount    float   chk-amount          t)
     (serial    string  chk-cheque-serial)
     (state-id  string  chk-cheque-state-id)
     (search    string)
     (cstate    string  chk-cheque-state-id)
     (since     date    chk-date)
     (until     date    chk-date))
  (with-controller-page (cheque/update kind)
    (check-cheque-accounts)
    (let* ((cheque-dao (get-dao 'cheque (val cheque-id)))
           (old-state-id (state-id cheque-dao))
           (new-state-id (if (or (string= "nil" (val state-id)) ; form with following states; no change
                                 (null (val state-id)))         ; no following states
                             old-state-id ;; unchanged
                             (val state-id))))
      ;; Don't touch company-id, state-id and payable-p
      (setf (bank-id cheque-dao) (bank-id (val bank))
            (company-id cheque-dao) (company-id (val company))
            (due-date cheque-dao) (val due-date)
            (amount cheque-dao) (val amount)
            (old-state-id cheque-dao) old-state-id
            (state-id cheque-dao) new-state-id
            (serial cheque-dao) (val serial))
      (update-dao cheque-dao)
      (see-other (apply #'cheque/details kind :cheque-id (val cheque-id) (params->filter))))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/delete (("cheque/" (kind "(receivable|payable)") "/delete"))
    ((cheque-id integer chk-cheque-id       t)
     (search    string)
     (cstate    string  chk-cheque-state-id)
     (since     date    chk-date)
     (until     date    chk-date))
  (with-view-page
    (check-cheque-accounts)
    (let* ((page-title (conc "Επιταγές » " (cheque-page-title kind) " » Διαγραφή"))
           (filter (params->filter))
           (cheque-table (make-instance 'cheque-table
                                        :kind kind
                                        :op :delete
                                        :selected-key (val cheque-id)
                                        :filter filter)))
      (with-document ()
        (:head
         (:title (str page-title))
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'cheque)
               (cheque-top-actions :delete)
               (filters cheque-table)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" (str page-title))
                           (actions cheque-table)
                           (with-form (actions/cheque/delete kind
                                                             :cheque-id (val cheque-id)
                                                             :search (val search)
                                                             :since (val since)
                                                             :until (val until)
                                                             :cstate (val cstate))
                             (display cheque-table))))
               (footer)))))))

(defpage cheque-page actions/cheque/delete
    (("actions/cheque/" (kind "(receivable|payable)") "/delete") :request-type :post)
    ((search    string)
     (cstate    string  chk-cheque-state-id)
     (since     date    chk-date)
     (until     date    chk-date)
     (cheque-id integer chk-cheque-id       t))
  (with-controller-page (cheque/delete kind)
    (check-cheque-accounts)
    (delete-dao (get-dao 'cheque (val cheque-id)))
    (see-other (apply #'cheque kind (params->filter)))))
