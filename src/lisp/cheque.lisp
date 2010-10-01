(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; Definition
;;; ------------------------------------------------------------

(defclass cheques-table (table-normal-crud)
  ((name :initform "cheques-table")
   (header :initform '(:id       ""
                       :bank     "Τράπεζα"
                       :due-date "Ημερομηνία πληρωμής"
                       :company  "Εταιρία"
                       :amount   "Ποσόν"
                       :status   "Κατάσταση"
                       :submit   ""
                       :cancel   ""))
   (styles :initform '(:active-row "active"
                       :inactive-row ""
                       :attention-row "attention"
                       :table "forms-in-row table-half"))
   ;; page interface
   (id-keys :initform '(:cheque-id))
   (data-keys :initform '(:bank
                          :due-date
                          :company
                          :amount
                          :status))
   (filter-keys :initform '(:payable-p))
   ;; crud mixin
   (main-page :initform 'cheques)
   (submit-pages :initform '(:create actions/cheque/create
                             :update actions/cheque/update
                             :delete actions/cheque/delete
                             :chstat actions/cheque/chstat))
   (cells-fn :initform (cheque-cells-fn))
   (data-fn :initform (cheque-data-fn))))

(defun make-cheques-table (&key operation params)
  (make-instance 'cheques-table
                 :operation operation
                 :params params))

(defun cheque-cells-fn ()
  (lambda (row)
    (destructuring-bind (&key cheque-id bank due-date company amount status) row
      (declare (ignore cheque-id))
      (with-db ()
        (let ((pairs (query (:select 'description 'status :from 'cheque-status))))
          (list (make-cell-selector :row row
                                    :name :id
                                    :style "select")
                (make-cell-textbox :row row
                                   :name :bank
                                   :value bank
                                   :style "data"
                                   :operations '(:create :update))
                (make-cell-textbox :row row
                                   :name :due-date
                                   :value due-date
                                   :style "data"
                                   :operations '(:create :update))
                (make-cell-textbox :row row
                                   :name :company
                                   :value company
                                   :style "data"
                                   :operations '(:create :update))
                (make-cell-textbox :row row
                                   :name :amount
                                   :value amount
                                   :style "data"
                                   :operations '(:create :update))
                (make-cell-dropdown :row row
                                    :name :status
                                    :value status
                                    :style "data"
                                    :operations '(:create :update)
                                    :pairs pairs)
                (make-cell-submit :row row
                                  :name :submit
                                  :style "button"
                                  :operations '(:create :update :delete))
                (make-cell-cancel :row row
                                  :name :cancel
                                  :style "button"
                                  :operations '(:create :update :delete))))))))

(defun cheque-data-fn ()
  (lambda (&key payable-p)
    (with-db ()
      (query (:select 'cheque.id 'bank.title 'company.title
                      'cheque.amount 'cheque.due-date
                      'company-id 'cheque.status
                      :from 'cheque
                      :left-join 'company
                      :on (:= 'company.id 'cheque.company-id)
                      :left-join 'bank
                      :on (:= 'bank.id 'cheque.bank-id)
                      :where (:= 'cheque.payable-p payable-p))
             :plists))))



;;; ------------------------------------------------------------
;;; Snippets
;;; ------------------------------------------------------------

(define-navbar cheque-navbar () (:id "subnavbar" :ul-style "hmenu")
  (receivable (cheques)              "Προς είσπραξη")
  (payable    (cheques :payable-p t) "Προς πληρωμή"))

(define-menu cheque-menu (cheque-id payable-p) ()
  (:create (with-html
             (:li (:a :href (cheque/create :payable-p payable-p)
                      (img "img/add.png") "Δημιουργία"))))
  (:paid (with-html
           (:li (:a :href (cheque/chstat :cheque-id cheque-id :new-status 'paid)
                    (img "img/magnifier.png") "Πληρωμή"))))
  (:bounced (with-html
              (:li (:a :href (cheque/chstat :cheque-id cheque-id :new-status 'bounced)
                       (img "img/magnifier.png") "Σφράγισμα"))))
  (:returned (with-html
               (:li (:a :href (cheque/chstat :cheque-id cheque-id :new-status 'returned)
                        (img "img/magnifier.png") "Επιστροφή"))))
  (:view (with-html
           (:li (:a :href (cheques :cheque-id cheque-id :payable-p payable-p)
                    (img "img/magnifier.png") "Προβολή"))))
  (:update (with-html
             (:li (:a :href (cheque/update :cheque-id cheque-id)
                      (img "img/pencil.png") "Επεξεργασία"))))
  (:delete (with-html
             (:li (:a :href (cheque/delete :cheque-id cheque-id)
                      (img "img/delete.png") "Διαγραφή")))))

(define-errorbar cheque-errorbar (:ul-style "error")
  (bank "Άκυρο όνομα τράπεζας")
  (company "Άκυρο όνομα εταιρίας")
  (amount "Άκυρο ποσό")
  (due-date "Άκυρη ημερομηνία"))

(defun cheque-statuses ()
  (with-db ()
    (query (:select 'description 'status :from 'cheque-status))))

(defun cheque-payable-p (cheque-id)
  (with-db ()
    (query (:select 'payable-p :from 'cheque :where (:= 'id cheque-id)) :single)))

(defun next-statuses (cheque-id)
  (if cheque-id
      (with-db ()
        (mapcar (compose #'make-keyword #'string-upcase)
                (query (:select 'new-status
                                :from 'cheque-stran
                                :where (:= 'old-status
                                           (:select 'status
                                                    :from 'cheque
                                                    :where (:= 'id cheque-id))))
                       :column)))
      nil))

(defun status-label (status)
  (first (find status (cheque-statuses) :key #'second :test #'string-equal)))


;;; ------------------------------------------------------------
;;; Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/cheque/create ((payable-p boolean)
                                            (bank string #'valid-bank-p t)
                                            (due-date date #'valid-due-date-p t)
                                            (company string #'valid-company-p t)
                                            (amount integer #'positive-p t))
    ("actions/cheque/create" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
        (with-parameter-rebinding #'val
          (with-db ()
            (let* ((status "pending") ;; default status for new cheques
                   (bank-id (bank-id bank))
                   (company-id (company-id company))
                   (stran-data (query (:select 'debit-acc-id 'credit-acc-id 'description
                                               :from 'cheque-stran
                                               :where (:and (:is-null 'old-status)
                                                            (:= 'new-status status)))
                                      :plist)))
              (break)
              (if stran-data
                  (with-transaction ()
                    ;; Store the dao of the new cheque, we need its id
                    ;; for the auto-generated transaction below
                    (let ((cheque-dao (insert-dao (make-instance 'cheque
                                                                 :bank-id bank-id
                                                                 :company-id company-id
                                                                 :due-date due-date
                                                                 :amount amount
                                                                 :status status
                                                                 :payable-p payable-p))))
                      (insert-dao (make-instance 'tx
                                                 :tx-date (current-date)
                                                 :description (getf stran-data :description)
                                                 :company-id company-id
                                                 :amount amount
                                                 :credit-acc-id (getf stran-data :credit-acc-id)
                                                 :debit-acc-id (getf stran-data :debit-acc-id)
                                                 :src-tbl "cheque"
                                                 :src-id (id cheque-dao)))
                      (see-other (cheques))))
                  (see-other (cheque/stran-notfound))))))
        (with-parameter-rebinding #'raw
          (see-other (cheque/create :bank bank
                                   :company company
                                   :due-date due-date
                                   :amount amount
                                   :payable-p payable-p))))))

(define-dynamic-page actions/cheque/delete ((cheque-id integer #'valid-cheque-id-p t))
    ("actions/cheque/delete" :request-type :post)
  (no-cache)
  (if (validp cheque-id)
      (with-parameter-rebinding #'val
        (with-db ()
          (with-transaction ()
            ;; First update /all/ transactions that have originated
            ;; from the cheque with this particular cheque-id
            (execute (:delete-from 'tx :where (:and (:= 'src-tbl "cheque")
                                                    (:= 'src-id cheque-id))))
            ;; Then delete the cheque itself
            (execute (:delete-from 'cheque :where (:= 'id cheque-id))))
          (see-other (cheques :payable-p (cheque-payable-p cheque-id)))))
      (see-other (cheque/notfound))))

(define-dynamic-page actions/cheque/update ((cheque-id integer #'valid-cheque-id-p t)
                                            (bank string #'valid-bank-p t)
                                            (company string #'valid-company-p t)
                                            (due-date date #'valid-due-date-p t)
                                            (amount integer #'positive-p t))
    ("actions/cheque/update" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
        (with-parameter-rebinding #'val
          (with-db ()
            (let* ((bank-id (bank-id bank))
                   (company-id (company-id company)))
              (with-transaction ()
                ;; First update /all/ transactions that have originated
                ;; from the cheque with this particular cheque-id
                (execute (:update 'tx :set
                                  'company-id company-id
                                  'amount amount
                                  :where (:and (:= 'src-tbl "cheque")
                                               (:= 'src-id cheque-id))))
                ;; Then update the cheque itself
                (execute (:update 'cheque :set
                                  'bank-id bank-id
                                  'company-id company-id
                                  'due-date due-date
                                  'amount amount
                                  :where (:= 'id cheque-id))))
              (see-other (cheques :cheque-id cheque-id :payable-p (cheque-payable-p cheque-id))))))
        (with-parameter-rebinding #'raw
          (see-other (cheque/update :cheque-id cheque-id
                                   :bank bank
                                   :company company
                                   :due-date due-date
                                   :amount amount))))))

(define-dynamic-page actions/cheque/chstat ((cheque-id integer #'valid-cheque-id-p)
                                            (new-status string))
    ("actions/cheque/chstat" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
        (with-parameter-rebinding #'val
          (with-db ()
            (bind (((old-status company-id amount) (query (:select 'status 'company-id 'amount
                                                                   :from 'cheque
                                                                   :where (:= 'id cheque-id))
                                                          :row))
                   (stran-data (query (:select 'debit-acc-id 'credit-acc-id 'description
                                               :from 'cheque-stran
                                               :where (:and (:= 'old-status old-status)
                                                            (:= 'new-status new-status)))
                                      :row)))
              (if stran-data
                  (destructuring-bind (debit-acc-id credit-acc-id description) stran-data
                    (with-transaction ()
                      ;; Create a new transaction which points to our cheque
                      (execute (:insert-into 'tx :set
                                             'tx-date (current-date)
                                             'description description
                                             'company-id company-id
                                             'amount amount
                                             'credit-acc-id credit-acc-id
                                             'debit-acc-id debit-acc-id
                                             'src-tbl "cheque"
                                             'src-id cheque-id))
                      ;; Then update the cheque itself
                      (execute (:update 'cheque :set
                                        'status new-status
                                        :where (:= 'id cheque-id))))
                    (see-other (cheques :cheque-id cheque-id
                                        :payable-p (cheque-payable-p cheque-id))))
                  (see-other (notfound))))))
        (with-parameter-rebinding #'raw
          (see-other (cheque/chstat :cheque-id cheque-id
                                   :new-status new-status))))))




;;; ------------------------------------------------------------
;;; Pages
;;; ------------------------------------------------------------
(define-dynamic-page cheques ((cheque-id integer #'valid-cheque-id-p)
                              (payable-p boolean))
    ("cheques" :validators (((cheque-id payable-p) ((lambda (cheque-id payable-p)
                                                      (with-db ()
                                                        (eql (payable-p
                                                              (get-dao 'cheque cheque-id))
                                                             payable-p)))
                                                    cheque-id
                                                    payable-p))))
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
        (let ((next-statuses (next-statuses (val cheque-id))))
          (with-page ()
            (:head
             (:title "Επιταγές")
             (head-config))
            (:body
             (:div :id "header"
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
             (footer))))
        (see-other (cheque/notfound)))))

(define-dynamic-page cheque/create ((payable-p boolean)
                                    (bank      string  #'valid-bank-p)
                                    (company   string  #'valid-company-p)
                                    (amount    integer #'positive-p)
                                    (due-date  date    #'valid-due-date-p))
    ("cheque/create")
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
