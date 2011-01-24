(in-package :scrooge)



;;; --------------------------------------------------------------------------------
;;; Validation
;;; --------------------------------------------------------------------------------

(define-existence-predicate cheque-id-exists-p cheque id)

(defun chk-cheque-id (id)
  (if (cheque-id-exists-p id)
      nil
      :cheque-id-unknown))



;;; --------------------------------------------------------------------------------
;;; Actions
;;; --------------------------------------------------------------------------------

(define-dynamic-page actions/cheque/create ("actions/cheque/create" :request-type :post)
    ((filter    string)
     (payable-p boolean)
     (bank      string  chk-bank-title)
     (due-date  date    nil               t)
     (company   string  chk-company-title t)
     (amount    integer chk-amount        t))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let ((bank-id (bank-id (val bank)))
              (company-id (company-id (val company)))
              (new-cheque (make-instance 'cheque
                                         :bank-id bank-id
                                         :company-id company-id
                                         :due-date (val due-date)
                                         :amount (val amount)
                                         :status (val status)
                                         :payable-p (val payable-p))))
          (with-db ()
            (insert-dao new-cheque)
            (see-other (cheques :id (id new-cheque) :filter (val filter)))))
        (see-other (cheque/create :filter (raw filter)
                                  :bank (raw bank)
                                  :company (raw company)
                                  :due-date (raw due-date)
                                  :amount (raw amount)
                                  :payable-p (raw payable-p))))))

(define-dynamic-page actions/cheque/update ("actions/cheque/update" :request-type :post)
    ((filter   string)
     (id       integer chk-cheque-id     t)
     (bank     string  chk-bank-title)
     (company  string  chk-company-title t)
     (due-date date    nil               t)
     (amount   integer chk-amount        t))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let* ((bank-id (bank-id (val bank)))
               (company-id (company-id (val company))))
          (with-db ()
            (execute (:update 'cheque :set
                              'bank-id bank-id
                              'company-id company-id
                              'due-date due-date
                              'amount amount
                              :where (:= 'id id))))
          (see-other (cheques :id id :payable-p (cheque-payable-p id))))
        (see-other (cheque/update :filter (raw filter)
                                  :id (raw id)
                                  :bank (raw bank)
                                  :company (raw company)
                                  :due-date (raw due-date)
                                  :amount (raw amount))))))

(define-dynamic-page actions/cheque/delete ("actions/cheque/delete" :request-type :post)
    ((id integer chk-cheque-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'cheque (val id)))
          (see-other (cheques :filter (val filter) :payable-p (cheque-payable-p id))))
        (see-other (notfound)))))



;;; ----------------------------------------------------------------------
;;; Cheque menu
;;; ----------------------------------------------------------------------

(defun cheque-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "cheque-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (standard-actions-spec (transaction :id id
                                                                    :filter filter)
                                                       (transaction/create :filter filter)
                                                       (transaction/update :id id
                                                                           :filter filter)
                                                       (transaction/delete :id id
                                                                           :filter filter)))
           :disabled-items disabled-items))



;;; ----------------------------------------------------------------------
;;; Database interface
;;; ----------------------------------------------------------------------

(defun get-cheque-plists (filter)
  (let* ((base-query `(:select cheque.id
                               :from cheque
                               :left-join bank
                               :on (:= bank.id cheque.bank-id)
                               :inner-join company
                               :on (:= company.id cheque.company-id)))
         (composite-query (if filter
                              (append base-query
                                      `(:where (:or (:ilike company.title ,(ilike filter))
                                                    (:ilike bank.title ,(ilike filter)))))
                              base-query))
         (final-query `(:order-by ,composite-query (:desc date))))
    (with-db ()
      (query (sql-compile final-query)
             :plists))))



;;; ----------------------------------------------------------------------
;;; Cheques table
;;; ----------------------------------------------------------------------

;;; table

(defclass cheque-table (crud-table)
  ((header-labels :initform '("" "Ημερομηνία πληρωμής" "Εταιρία" "Ποσό"))
   (paginator     :initform (make-instance 'paginator
                                           :id "tx-paginator"
                                           :style "paginator grid_9 alpha"
                                           :delta 10
                                           :urlfn (lambda (filter start)
                                                    (cheque :filter filter
                                                            :start start))))))



(defun read-items ((table cheque-table))
  (iter (for rec in (geet-cheque-plists (filter table)))
        (for i from 0)
        (collect (make-instance 'cheque-row
                                :key (getf rec :id)
                                :record rec
                                :collection table
                                :index i))))


;;; rows

(defclass cheque-row (crud-row)
  ())

(defmethod cells ((row cheque-row) &key start)
  (let* ((id (key row))
         (record (record row))
         (pg (paginator (collection row)))
         (filter (filter (collection row))))
    (list :selector
          (make-instance 'selector-cell
                         :states (list :on (transaction :filter filter
                                                        :start (page-start pg (index row) start))
                                       :off (transaction :filter filter
                                                         :id id)))
          :payload
          (mapcar (lambda (name)
                    (make-instance 'textbox-cell
                                   :name name
                                   :value (getf record (make-keyword name))))
                  '(date company amount))
          :controls
          (list
           (make-instance 'ok-cell)
           (make-instance 'cancel-cell
                          :href (cheque :id id :filter filter))))))



;;; ------------------------------------------------------------
;;; Notifications
;;; ------------------------------------------------------------


(defun  cheque-notifications (&rest params)
  (notifications
   '((company (:company-title-unknown "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"))
     (amount (:non-positive-amount  "Το ποσό της συναλλαγής πρέπει να είναι θετικός αριθμός"
              :invalid-amount  "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες")))))



;;; ------------------------------------------------------------
;;; Pages
;;; ------------------------------------------------------------

(define-dynamic-page cheque ("cheque")
    ((filter    string)
     (payable-p boolean)
     (bank      string  chk-bank-title)
     (due-date  date    nil               t)
     (company   string  chk-company-title t)
     (amount    integer chk-amount        t))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp parameters)
        (with-page ()
          (:head
           (:title "Επιταγές")
           (admin-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (logo)
                 (primary-navbar 'cheques)
                 (cheque-navbar (if payable-p 'payable 'receivable)))
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Κατάλογος επιταγών"))
                 (:div :id "cheques" :class "window"
                       (apply #'cheque-menu (val cheque-id) (val payable-p)
                              :create :update :delete next-statuses)
                       (render (make-cheques-table :operation :view
                                                   :params params))))
           (footer)))
        (see-other (cheque/notfound)))))

(define-dynamic-page cheque/create ("cheque/create")
    ((filter    string)
     (payable-p boolean)
     (bank      string  chk-bank-title)
     (due-date  date    nil               t)
     (company   string  chk-company-title t)
     (amount    integer chk-amount        t))
  (no-cache)
  (with-parameter-list params
    (with-page ()
      (:head
       (:title "Επιταγές")
       (head-config))
      (:body
       (:div :id "header"
             (logo)
             (primary-navbar 'cheques)
             (cheque-navbar (if (val payable-p) 'payable 'receivable)))
       (:div :id "body"
             (:div :id "message"
                   (:h2 :class "info" "Δημιουργία επιταγής")
                   (cheque-errorbar bank company amount due-date))
             (:div :id "cheques" :class "window"
                   (cheque-menu nil (val payable-p))
                   (render (make-cheques-table :operation :create
                                               :params params)))
             (footer))))))

(define-dynamic-page cheque/update ((cheque-id integer #'valid-cheque-id-p t)
                                    (bank string #'valid-bank-p)
                                    (company string #'valid-company-p)
                                    (amount integer #'positive-p)
                                    (due-date date #'valid-due-date-p))
    ("cheque/update")
  (no-cache)
  (if (validp cheque-id)
      (let ((payable-p (cheque-payable-p (val cheque-id))))
        (with-parameter-list params
          (with-page ()
            (:head
             (:title "Επιταγές")
             (head-css-std)
             (head-js-std))
            (:body
             (:div :id "header"
                   (logo)
                   (primary-navbar 'cheques)
                   (cheque-navbar (if payable-p 'payable 'receivable)))
             (:div :id "body"
                   (cheque-errorbar bank company amount due-date)
                   (:div :id "cheques" :class "window"
                         (cheque-menu (val cheque-id)
                                      payable-p
                                      :view :delete)
                         (:h2 "Επεξεργασία επιταγής")
                         (render (make-cheques-table :operation :update
                                                     :params params)))
                   (footer))))))
      (see-other (cheque/notfound))))

(define-dynamic-page cheque/delete ((cheque-id integer #'valid-cheque-id-p t))
    ("cheque/delete")
  (no-cache)
  (if (validp cheque-id)
      (with-parameter-list params
        (let ((payable-p (cheque-payable-p (val cheque-id))))
          (with-page ()
            (:head
             (:title "Επιταγές")
             (head-css-std))
            (:body
             (:div :id "header"
                   (logo)
                   (primary-navbar 'cheques)
                   (cheque-navbar (if payable-p 'payable 'receivable)))
             (:div :id "body"
                   (:div :id "cheques" :class "window"
                         (cheque-menu (val cheque-id) payable-p :view :update)
                         (:h2 "Διαγραφή επιταγής")
                         (render (make-cheques-table :operation :delete
                                                     :params params)))
                   (footer))))))
      (see-other (cheque/notfound))))

(define-dynamic-page cheque/chstat ((cheque-id integer #'valid-cheque-id-p t)
                                    (new-status symbol #'valid-cheque-status-p t))
    ("cheque/chstat")
  (no-cache)
  (if (and (validp cheque-id) (validp new-status))
      (with-parameter-list params
        (let ((payable-p (cheque-payable-p (val cheque-id))))
          (with-page ()
            (:head
             (:title "Επιταγές")
             (head-css-std))
            (:body
             (:div :id "header"
                   (logo)
                   (primary-navbar 'cheques)
                   (cheque-navbar (if payable-p 'payable 'receivable)))
             (:div :id "body"
                   (:div :id "cheques" :class "window"
                         (cheque-menu (val cheque-id) (val payable-p) :view)
                         (:h2 (str (case new-status
                                     (paid "Πληρωμή επιταγής")
                                     (bounced "Σφράγισμα επιταγής")
                                     (returned "Επιστροφή επιταγής"))))
                         (render (make-cheques-table :operation :chstat
                                                     :params params)))
                   (footer))))))
      (see-other (cheque/notfound))))

(define-dynamic-page cheque/notfound () ("cheque/notfound")
  (no-cache)
  (with-page ()
    (:head
     (:title "Άγνωστη εταιρία")
     (head-css-std))
    (:body
     (:div :id "header"
           (logo)
           (primary-navbar 'companies))
     (:div :id "body"
           (:div :id "content" :class "window"
                 (:p "Η επιταγή που προσπαθείτε να προσπελάσετε δεν υπάρχει.")
                 (:p "Επιστρέψτε στο μενού των επιταγών και προσπαθήστε ξανά."))))))


(define-dynamic-page cheque/stran-notfound () ("cheque/stran-not-found")
  (no-cache)
  (with-page ()
    (:head
     (:title "Άγνωστη σελίδα")
     (head-css-std))
    (:body
     (:div :id "header"
           (logo)
           (primary-navbar 'cheques))
     (:div :id "body"
           (:div :id "content" :class "summary"
                 (:p "Δεν έχουν δημιουργηθεί οι κατάλληλες καταστατικές μεταβολές.")
                 (:p "Επιστρέψτε στο μενού των καταστατικών μεταβολών και ορίστε τις."))))))
