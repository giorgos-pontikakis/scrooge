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
                  "Η επωνυμία της εταιρίας είναι κενή"
                  :company-immediate-tx-only
                  "Επιτρέπονται μόνο συναλλαγές απ' ευθείας εξόφλησης (όχι έναντι ανοιχτού λογαριασμού) με αυτή την εταιρία"
                  :company-supplier-only
                  "Αυτή η εταιρία δεν μπορεί να εμφανίζει έσοδα."
                  :company-customer-only
                  "Αυτή η εταιρία δεν μπορεί να εμφανίζει έξοδα."))
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
  (unless (and (account-id 'cheque-receivable-account)
               (account-id 'cheque-payable-account)
               (account-id 'revenues-root-account)
               (account-id 'expenses-root-account))
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
;;; Utilities
;;; ----------------------------------------------------------------------

(defun cheque-page-title (role op-label)
  (conc "Επιταγές » " (if (customer-p role) "Εισπρακτέες" "Πληρωτέες") " » " op-label))

(defun following-cheque-states (from-state-id customer-p)
  (lists->alist
   (query (:select 'cheque-state.id 'cheque-state.description
                   :from 'cheque-state
                   :inner-join 'cheque-stran
                   :on (:= 'cheque-state.id 'cheque-stran.to-state-id)
                   :where (:or (:and (:= 'cheque-stran.from-state-id from-state-id)
                                     (:= 'cheque-stran.customer-p customer-p))
                               (:and (:= 'cheque-state.id from-state-id)
                                     (:= 'cheque-stran.customer-p customer-p)))))))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun cheque-top-actions (op)
  (let ((role (first *registers*)))
    (top-actions-area
     (make-instance 'scrooge-menu
                    :spec (make-menu-spec
                           (list :catalogue (family-url 'cheque :system :filter)
                                 :create (list (family-url 'cheque/create :filter)
                                               (conc "Νέα "
                                                     (if (customer-p role)
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
;;; Database interface
;;; ----------------------------------------------------------------------

(defun get-cheque-records (table &optional company-id)
  (let* ((search (getf (filter table) :search))
         (since (getf (filter table) :since))
         (until (getf (filter table) :until))
         (cstate (getf (filter table) :cstate))
         (customer-p (customer-p (role table)))
         (base-query `(:select cheque.id (:as bank.title bank) serial state-id
                               (:as company.title company) company-id
                               (:as cheque-state.description state-description)
                               due-date amount customer-p
                               :from cheque
                               :left-join bank
                               :on (:= bank.id cheque.bank-id)
                               :inner-join company
                               :on (:= company.id cheque.company-id)
                               :inner-join cheque-state
                               :on (:= cheque-state.id cheque.state-id)))
         (sort-order (if (string= cstate *default-cheque-state-id*)
                         '(due-date company)
                         '((:desc 'due-date) company)))
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
    (when (role table)
      (push `(:= cheque.customer-p ,customer-p) where))
    (let ((sql `(:order-by (,@base-query :where
                                         (:and
                                          ,@where))
                           ,@sort-order)))
      (query (sql-compile sql)
             :plists))))



;;; ----------------------------------------------------------------------
;;; Cheque table
;;; ----------------------------------------------------------------------

;;; table

(defclass cheque-table (scrooge-table)
  ((role :accessor role :initarg :role)
   (paginator :accessor paginator :initarg :paginator)
   (header-labels :initform '("" "Σειριακός<br />Αριθμός" "<br />Εταιρία" "<br />Τράπεζα"
                              "Ημερομηνία<br />λήξης" "<br />Ποσό")))
  (:default-initargs :item-class 'cheque-row
                     :id "cheque-table"
                     :role nil
                     :paginator (make-instance 'cheque-paginator
                                               :id "cheque-paginator"
                                               :css-class "paginator")))

(defmethod get-records ((table cheque-table))
  (get-cheque-records table))

(defmethod actions ((tbl cheque-table) &key)
  (let* ((cheque-id (selected-key tbl))
         (role (role tbl))
         (filter (filter tbl))
         (hrefs (if cheque-id
                    (list :update (apply #'cheque/update role :cheque-id cheque-id filter)
                          :delete (apply #'cheque/delete role :cheque-id cheque-id filter))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))

(defmethod filters ((tbl cheque-table))
  (let* ((role (role tbl))
         (filter (filter tbl))
         (filter* (remove-from-plist filter role :cstate))
         (filter-spec `((nil      ,(apply #'cheque role filter*)
                                  "Όλες")
                        (pending  ,(apply #'cheque role :cstate "pending" filter*)
                                  "Σε εκκρεμότητα")
                        (paid     ,(apply #'cheque role :cstate "paid" filter*)
                                  "Πληρωμένες")
                        (bounced  ,(apply #'cheque role :cstate "bounced" filter*)
                                  "Ακάλυπτες")
                        (returned ,(apply #'cheque role :cstate "returned" filter*)
                                  "Επιστραμμένες")
                        (stamped  ,(apply #'cheque role :cstate "stamped" filter*)
                                  "Σφραγισμένες"))))
    (filter-area (filter-navbar `((customer ,(apply #'cheque "customer" filter) "Προς είσπραξη")
                                  (supplier ,(apply #'cheque "supplier" filter) "Προς πληρωμή"))
                                :active role
                                :id "cheque-role-navbar")
                 (datebox (lambda (&rest args)
                            (apply #'cheque role args))
                          filter)
                 (filter-navbar filter-spec
                                :active (getf filter :cstate)))))


;;; rows

(defclass cheque-row (scrooge-row/plist)
  ())

(defmethod selector ((row cheque-row) selected-p)
  (let* ((cheque-id (key row))
         (table (collection row))
         (pg (paginator table))
         (filter (filter table))
         (role (role table))
         (start (start-index table)))
    (html ()
      (:a :href (if selected-p
                    (apply #'cheque role
                           :start (page-start pg (index row) start)
                           filter)
                    (apply #'cheque role :cheque-id cheque-id filter))
        (selector-img selected-p)))))

(defmethod payload ((row cheque-row) enabled-p)
  (let* ((record (record row)))
    (list (make-instance 'textbox
                         :name 'serial
                         :value (getf record :serial)
                         :disabled (not enabled-p))
          (make-instance 'textbox
                         :name 'company
                         :value (getf record :company)
                         :css-class "ac-company"
                         :disabled (not enabled-p)
                         :href (company/details :company-id (getf record :company-id)))
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
                         :disabled (not enabled-p))
          (if (and enabled-p (eql (op (collection row)) :update))
              (make-instance 'dropdown
                             :name 'state-id
                             :selected (getf record :state-id)
                             :value-label-alist (following-cheque-states (getf record :state-id)
                                                                         (getf record :customer-p)))
              (make-instance 'textbox
                             :name 'state-id
                             :value (or (getf record :state-description)
                                        (description (get-dao 'cheque-state *default-cheque-state-id*)))
                             :disabled t)))))

(defmethod controls ((row cheque-row) controls-p)
  (let* ((cheque-id (key row))
         (table (collection row))
         (filter (filter table))
         (role (role table)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (apply #'cheque role :cheque-id cheque-id filter)))
        (list nil nil))))


;;; paginator

(defclass cheque-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg cheque-paginator) start)
  (let ((table (table pg)))
    (apply #'cheque (role table) :start start (filter table))))



;;; ------------------------------------------------------------
;;; SEARCH
;;; ------------------------------------------------------------

(defpage cheque-page actions/cheque/search
    (("actions/cheque/" (role "(customer|supplier)") "/search") :request-type :get)
    ((search string)
     (cstate string chk-cheque-state-id)
     (since  date   chk-date)
     (until  date   chk-date))
  (with-db ()
    (let* ((filter (params->filter))
           (rows (rows (make-instance 'cheque-table :op :catalogue
                                                    :role role
                                                    :filter filter))))
      (if (single-item-list-p rows)
          (see-other (apply #'cheque role
                            :cheque-id (key (first rows))
                            filter))
          (see-other (apply #'cheque role filter))))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage cheque-page cheque (("cheque/" (role "(customer|supplier)")))
    ((cheque-id integer chk-cheque-id)
     (start     integer)
     (search    string)
     (cstate    string  chk-cheque-state-id)
     (since     date    chk-date)
     (until     date    chk-date))
  (check-cheque-accounts)
  (with-view-page
    (let* ((filter (params->filter))
           (page-title (cheque-page-title role "Κατάλογος"))
           (cheque-table (make-instance 'cheque-table
                                        :id "cheque-table"
                                        :role role
                                        :op :catalogue
                                        :selected-key (val cheque-id)
                                        :filter filter
                                        :start-index (val start))))
      ;; if cheque-id exists and is not found among records, ignore search term
      (when (and (val cheque-id)
                 (not (find (val cheque-id) (rows cheque-table) :key #'key)))
        (let ((dao (get-dao 'cheque (val cheque-id))))
          (see-other (cheque (if (customer-p dao) "customer" "supplier")
                             :cheque-id (val cheque-id)
                             :cstate (state-id dao)))))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'cheque)
            (cheque-top-actions :catalogue)
            (filters cheque-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" (str page-title))
                (actions cheque-table)
                (display cheque-table)))
            (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/create (("cheque/" (role  "(customer|supplier)") "/create"))
    ((bank     string chk-bank-title)
     (due-date date   chk-date)
     (company  string chk-company-title)
     (amount   float  chk-amount)
     (serial   string chk-cheque-serial)
     (search   string)
     (cstate   string chk-cheque-state-id)
     (since    date   chk-date)
     (until    date   chk-date))
  (validate-parameters (chk-tx-constraints-fn role) company)
  (check-cheque-accounts)
  (with-view-page
    (let* ((filter (params->filter))
           (page-title (cheque-page-title role "Δημιουργία"))
           (cheque-table (make-instance 'cheque-table
                                        :role role
                                        :op :create
                                        :filter filter)))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'cheque)
            (cheque-top-actions :create)
            (filters cheque-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" (str page-title))
                (actions cheque-table)
                (notifications)
                (with-form (actions/cheque/create role
                                                  :search (val search)
                                                  :since (val since)
                                                  :until (val until)
                                                  :cstate (val cstate))
                  (display cheque-table :payload (params->payload)))))
            (footer)))))))

(defpage cheque-page actions/cheque/create
    (("actions/cheque/" (role "(customer|supplier)") "/create") :request-type :post)
    ((search   string)
     (cstate   string chk-cheque-state-id)
     (since    date   chk-date)
     (until    date   chk-date)
     (bank     string chk-bank-title)
     (serial   string chk-cheque-serial)
     (company  string chk-company-title   t)
     (due-date date   chk-date            t)
     (amount   float  chk-amount          t))
  (validate-parameters (chk-tx-constraints-fn role) company)
  (check-cheque-accounts)
  (with-controller-page (cheque/create role)
    (let ((new-cheque (make-instance 'cheque
                                     :serial (val serial)
                                     :bank-id (bank-id (val bank))
                                     :company-id (company-id (val company))
                                     :due-date (val due-date)
                                     :amount (val amount)
                                     :customer-p (customer-p role)
                                     :state-id *default-cheque-state-id*)))
      (insert-dao new-cheque)
      (see-other (apply #'cheque role :cheque-id (cheque-id new-cheque) (params->filter))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/update (("cheque/" (role "(customer|supplier)") "/update"))
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
  (validate-parameters (chk-tx-constraints-fn role) company)
  (check-cheque-accounts)
  (with-view-page
    (let* ((filter (params->filter))
           (cheque-table (make-instance 'cheque-table
                                        :role role
                                        :op :update
                                        :selected-key (val cheque-id)
                                        :filter filter))
           (page-title (cheque-page-title role "Επεξεργασία")))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'cheque)
            (cheque-top-actions :update)
            (filters cheque-table)
            (:div :class "grid_12"
              (:div :id "cheque-window" :class "window"
                (:p :class "title" (str page-title))
                (actions cheque-table)
                (notifications)
                (with-form (actions/cheque/update role
                                                  :cheque-id (val cheque-id)
                                                  :search (val search)
                                                  :since (val since)
                                                  :until (val until)
                                                  :cstate (val cstate))
                  (display cheque-table :payload (params->payload)))))
            (footer)))))))

(defpage cheque-page actions/cheque/update
    (("actions/cheque/" (role "(customer|supplier)") "/update") :request-type :post)
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
  (validate-parameters (chk-tx-constraints-fn role) company)
  (check-cheque-accounts)
  (with-controller-page (cheque/update role)
    (let* ((cheque-dao (get-dao 'cheque (val cheque-id)))
           (old-state-id (state-id cheque-dao))
           (new-state-id (val state-id)))
      (setf (bank-id cheque-dao) (bank-id (val bank))
            (company-id cheque-dao) (company-id (val company))
            (due-date cheque-dao) (val due-date)
            (amount cheque-dao) (val amount)
            (old-state-id cheque-dao) old-state-id
            (state-id cheque-dao) new-state-id
            (serial cheque-dao) (val serial))
      (update-dao cheque-dao)
      (see-other (apply #'cheque role :cheque-id (val cheque-id) (params->filter))))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage cheque-page cheque/delete (("cheque/" (role "(customer|supplier)") "/delete"))
    ((cheque-id integer chk-cheque-id       t)
     (search    string)
     (cstate    string  chk-cheque-state-id)
     (since     date    chk-date)
     (until     date    chk-date))
  (check-cheque-accounts)
  (with-view-page
    (let* ((page-title (cheque-page-title role "Διαγραφή"))
           (filter (params->filter))
           (cheque-table (make-instance 'cheque-table
                                        :role role
                                        :op :delete
                                        :selected-key (val cheque-id)
                                        :filter filter)))
      (with-document ()
        (:head
          (:title (str page-title))
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'cheque)
            (cheque-top-actions :delete)
            (filters cheque-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" (str page-title))
                (actions cheque-table)
                (with-form (actions/cheque/delete role
                                                  :cheque-id (val cheque-id)
                                                  :search (val search)
                                                  :since (val since)
                                                  :until (val until)
                                                  :cstate (val cstate))
                  (display cheque-table))))
            (footer)))))))

(defpage cheque-page actions/cheque/delete
    (("actions/cheque/" (role "(customer|supplier)") "/delete") :request-type :post)
    ((search    string)
     (cstate    string  chk-cheque-state-id)
     (since     date    chk-date)
     (until     date    chk-date)
     (cheque-id integer chk-cheque-id       t))
  (check-cheque-accounts)
  (with-controller-page (cheque/delete role)
    (delete-dao (get-dao 'cheque (val cheque-id)))
    (see-other (apply #'cheque role (params->filter)))))
