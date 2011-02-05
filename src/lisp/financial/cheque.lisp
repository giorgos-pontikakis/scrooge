(in-package :scrooge)



;;; --------------------------------------------------------------------------------
;;; Validation
;;; --------------------------------------------------------------------------------

(define-existence-predicate cheque-id-exists-p cheque id)
(define-existence-predicate cheque-status-exists-p cheque-status id)

(defun chk-cheque-id (id)
  (if (cheque-id-exists-p id)
      nil
      :cheque-id-unknown))

(defun chk-cheque-status (status)
  (if (cheque-status-exists-p status)
      nil
      :cheque-status-invalid))

(defun chk-date (date)
  (if (eql :null date)
      :date-null
      nil))



;;; --------------------------------------------------------------------------------
;;; Actions
;;; --------------------------------------------------------------------------------

(define-regex-page actions/financial/cheque/create
    (("actions/financial/cheque/" cheque-kind "/create")
     :registers (cheque-kind "(receivable|payable)")
     :request-type :post)
    ((search   string)
     (bank     string chk-bank-title)
     (company  string chk-company-title t)
     (due-date date   chk-date          t)
     (amount   string chk-amount        t)
     (status   string chk-cheque-status t))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let* ((bank-id (bank-id (val bank)))
               (company-id (company-id (val company)))
               (new-cheque (make-instance 'cheque
                                          :bank-id bank-id
                                          :company-id company-id
                                          :due-date (val due-date)
                                          :amount (val amount)
                                          :status (val status)
                                          :payable-p (string= cheque-kind "payable"))))
          (with-db ()
            (insert-dao new-cheque)
            (see-other (cheque cheque-kind :id (id new-cheque) :search (val search)))))
        (see-other (cheque/create cheque-kind
                                  :search (raw search)
                                  :bank (raw bank)
                                  :company (raw company)
                                  :due-date (raw due-date)
                                  :amount (raw amount))))))

(define-regex-page actions/financial/cheque/update
    (("actions/financial/cheque/" cheque-kind "/update")
     :registers (cheque-kind "(receivable|payable)")
     :request-type :post)
    ((search   string)
     (id       integer chk-cheque-id     t)
     (bank     string  chk-bank-title)
     (company  string  chk-company-title t)
     (due-date date    chk-date          t)
     (amount   string  chk-amount        t)
     (status   string  chk-cheque-status t))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let* ((bank-id (bank-id (val bank)))
               (company-id (company-id (val company))))
          (with-db ()
            (execute (:update 'cheque :set
                              'bank-id bank-id
                              'company-id company-id
                              'due-date (val due-date)
                              'amount (val amount)
                              'status (val status)
                              :where (:= 'id (val id)))))
          (see-other (cheque cheque-kind :id (val id))))
        (see-other (cheque/update cheque-kind
                                  :search (raw search)
                                  :id (raw id)
                                  :bank (raw bank)
                                  :company (raw company)
                                  :due-date (raw due-date)
                                  :amount (raw amount)
                                  :status (raw status))))))

(define-regex-page actions/financial/cheque/delete
    (("actions/financial/cheque/" cheque-kind "/delete")
     :registers (cheque-kind "(receivable|payable)")
     :request-type :post)
    ((search string)
     (id     integer chk-cheque-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'cheque (val id)))
          (see-other (cheque cheque-kind :search (val search))))
        (see-other (notfound)))))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun cheque-menu (cheque-kind id search &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "cheque-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (crud+details-actions-spec (cheque cheque-kind
                                                                   :id id
                                                                   :search search)
                                                           (cheque/create cheque-kind
                                                                          :search search)
                                                           (cheque/details cheque-kind
                                                                           :id id
                                                                           :search search)
                                                           (cheque/update cheque-kind
                                                                          :id id
                                                                          :search search)
                                                           (cheque/delete cheque-kind
                                                                          :id id
                                                                          :search search)))
           :disabled-items disabled-items))

(defun  cheque-notifications ()
  (notifications
   '((due-date (:date-null "Η ημερομηνία είναι κενή"))
     (bank (:bank-title-null "Το όνομα της τράπεζας είναι κενό."
            :bank-title-unknown "Δεν έχει καταχωρηθεί τράπεζα με αυτή την επωνυμία"))
     (company (:company-title-null "Το όνομα της εταιρίας είναι κενό"
               :company-title-unknown "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"))
     (amount (:non-positive-amount  "Το ποσό της συναλλαγής πρέπει να είναι θετικός αριθμός"
              :invalid-amount  "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες")))))

(defun cheque-filters (cheque-kind)
  (with-html
    (:div :id "filters"
          (display (make-instance 'vertical-navbar
                                  :id "cheque-kind-filter"
                                  :style "vnavbar"
                                  :spec `((receivable ,(cheque "receivable") "Εισπρακτέες")
                                          (payable    ,(cheque "payable")    "Πληρωτέες")))
                   :active-page-name (intern (string-upcase cheque-kind))))))


;;; ----------------------------------------------------------------------
;;; Database interface
;;; ----------------------------------------------------------------------

(defun get-cheque-plist (id)
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

(defclass cheque-table (crud-table)
  ((header-labels :initform '("" "<br />Τράπεζα" "Ημερομηνία<br />πληρωμής"
                              "<br />Εταιρία" "<br />Ποσό"))
   (paginator     :initform (make-instance 'paginator
                                           :id "tx-paginator"
                                           :style "paginator grid_9 alpha"
                                           :delta 10
                                           :urlfn (lambda (cheque-kind search start)
                                                    (cheque cheque-kind
                                                            :search search
                                                            :start start)))))
  (:default-initargs :item-class 'cheque-row))

(defmethod read-records ((table cheque-table))
  (let* ((filter (filter table))
         (search (getf filter :search))
         (payable-p (string= (getf filter :cheque-kind) "payable"))
         (base-query `(:select cheque.id (:as bank.title bank)
                               due-date (:as company.title company) amount payable-p
                               :from cheque
                               :inner-join bank
                               :on (:= bank.id cheque.bank-id)
                               :inner-join company
                               :on (:= company.id cheque.company-id)))
         (composite-query (if filter
                              (append base-query
                                      `(:where (:and (:or (:ilike company.title ,(ilike search))
                                                          (:ilike bank.title ,(ilike search)))
                                                     (:= cheque.payable-p ,payable-p))))
                              base-query))
         (final-query `(:order-by ,composite-query (:desc due-date))))
    (with-db ()
      (query (sql-compile final-query)
             :plists))))


;;; rows

(defclass cheque-row (crud-row)
  ())

(defmethod cells ((row cheque-row) &key start)
  (let* ((id (key row))
         (record (record row))
         (pg (paginator (collection row)))
         (search (getf (filter (collection row)) :search))
         (cheque-kind (if (getf record :payable-p) "payable" "receivable")))
    (list :selector
          (make-instance 'selector-cell
                         :states (list :on (cheque cheque-kind
                                                   :search search
                                                   :start (page-start pg (index row) start))
                                       :off (cheque cheque-kind
                                                    :search search
                                                    :id id)))
          :payload
          (mapcar (lambda (name)
                    (make-instance 'textbox-cell
                                   :name name
                                   :value (getf record (make-keyword name))))
                  '(bank due-date company amount))
          :controls
          (list
           (make-instance 'ok-cell)
           (make-instance 'cancel-cell
                          :href (cheque cheque-kind :id id :search search))))))



;;; ------------------------------------------------------------
;;; Pages
;;; ------------------------------------------------------------

(define-regex-page cheque (("financial/cheque/" cheque-kind)
                           :registers (cheque-kind "(receivable|payable)"))
    ((search    string)
     (start     integer)
     (id        integer chk-cheque-id))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (let ((cheque-table (make-instance 'cheque-table
                                           :id "cheque-table"
                                           :op 'catalogue
                                           :filter (list :search (val* search)
                                                         :cheque-kind cheque-kind))))
          (with-document ()
            (:head
             (:title "Επιταγές")
             (financial-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'financial)
                   (financial-navbar 'cheque)
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
                         (searchbox (cheque cheque-kind) (val search))
                         (cheque-filters cheque-kind))
                   (:div :class "window grid_9"
                         (:div :class "title" "Κατάλογος επιταγών")
                         (cheque-menu cheque-kind
                                      (val id)
                                      (val search)
                                      (if (val id)
                                          '(catalogue create)
                                          '(catalogue details update delete)))
                         (display cheque-table
                                  :selected-id (val* id)
                                  :selected-data nil
                                  :start (val* start)))
                   (footer)))))
        (see-other (notfound)))))

(define-regex-page cheque/create (("financial/cheque/" cheque-kind "/create")
                                  :registers (cheque-kind "(receivable|payable)"))
    ((search    string)
     (bank      string  chk-bank-title)
     (due-date  date    chk-date)
     (company   string  chk-company-title)
     (amount    string  chk-amount)
     (status    string  chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
    (with-document ()
      (:head
       (:title "Επιταγές > Δημιουργία")
       (financial-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'financial)
             (financial-navbar 'cheque)
             (:div :id "sidebar" :class "sidebar grid_3"
                   (:p :class "title" "Φίλτρα")
                   (searchbox (cheque cheque-kind) (val search))
                   (cheque-notifications))
             (:div :class "window grid_9"
                   (:div :class "title" "Δημιουργία επιταγής")
                   (cheque-menu cheque-kind
                                nil
                                (val search)
                                '(create update delete))
                   (:form :action (actions/financial/cheque/create cheque-kind :search (val* search))
                          :method :post
                          (cheque-data-form cheque-kind
                                            'create
                                            :search (val search)
                                            :data (parameters->plist bank
                                                                     company
                                                                     due-date
                                                                     amount
                                                                     status)
                                            :styles (parameters->styles bank
                                                                        company
                                                                        due-date
                                                                        amount
                                                                        status)))))
       (footer)))))

(define-regex-page cheque/update (("financial/cheque/" cheque-kind "/update")
                                  :registers (cheque-kind "(receivable|payable)"))
    ((search   string)
     (id       integer chk-cheque-id     t)
     (bank     string  chk-bank-title)
     (company  string  chk-company-title)
     (due-date date    chk-date)
     (status   string  chk-cheque-status)
     (amount   string  chk-amount))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-document ()
          (:head
           (:title "Επιταγές > Επεξεργασία")
           (financial-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'financial)
                 (financial-navbar 'cheque)
                 (:div :id "sidebar" :class "sidebar grid_3"
                       (:p :class "title" "Φίλτρα")
                       (searchbox (cheque cheque-kind) (val search))
                       (cheque-notifications))
                 (:div :class "window grid_9"
                       (:div :class "title" "Επεξεργασία επιταγής")
                       (cheque-menu cheque-kind
                                    (val id)
                                    (val search)
                                    '(create update))
                       (with-form (actions/financial/cheque/update cheque-kind :id (val id))
                         (cheque-data-form cheque-kind
                                           'update
                                           :search (val search)
                                           :data (plist-union (parameters->plist bank
                                                                                 company
                                                                                 due-date
                                                                                 amount
                                                                                 status)
                                                              (get-cheque-plist (val id)))
                                           :styles (parameters->styles bank
                                                                       company
                                                                       due-date
                                                                       amount
                                                                       status)))
                       (footer)))))
        (see-other (notfound)))))

(define-regex-page cheque/details (("financial/cheque/" cheque-kind "/details")
                                   :registers (cheque-kind "(receivable|payable)"))
    ((search     string)
     (id         integer chk-cheque-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-document ()
          (:head
           (:title "Επιταγές > Λεπτομέρειες")
           (financial-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'financial)
                 (financial-navbar 'cheque)
                 (:div :class "window grid_9"
                       (:div :class "title" "Λεπτομέρειες επιταγής")
                       (cheque-menu cheque-kind
                                    (val id)
                                    (val search)
                                    '(details create))
                       (cheque-data-form cheque-kind
                                         'details
                                         :search (val search)
                                         :id (val id)
                                         :data (get-cheque-plist (val id))))
                 (:div :id "sidebar" :class "sidebar grid_3"
                       "")
                 (error-page))))
        (see-other (notfound)))))

(define-regex-page cheque/delete (("financial/cheque/" cheque-kind "/delete")
                                  :registers (cheque-kind "(receivable|payable)"))
    ((search string)
     (id     integer chk-cheque-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((cheque-table (make-instance 'cheque-table
                                           :op 'delete
                                           :id "cheque-table"
                                           :filter (list :search (val* search)
                                                         :cheque-kind cheque-kind))))
          (with-document ()
            (:head
             (:title "Επιταγές > Διαγραφή")
             (financial-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'financial)
                   (financial-navbar 'cheque)
                   (:div :class "window"
                         (:div :id "sidebar" :class "sidebar grid_3"
                               (:p :class "title" "Φίλτρα")
                               (searchbox (cheque cheque-kind) (val search)))
                         (:div :class "window grid_9"
                               (:div :class "title" "Διαγραφή επιταγής")
                               (cheque-menu cheque-kind
                                            (val id)
                                            (val search)
                                            '(catalogue create delete))
                               (with-form (actions/financial/cheque/delete cheque-kind
                                                                           :id (val id)
                                                                           :search (val* search))
                                 (display cheque-table
                                          :selected-id (val id)))))
                   (footer)))))
        (see-other (notfound)))))

(defun cheque-data-form (cheque-kind op &key search id data styles)
  (let ((disabledp (eql op 'details)))
    (flet ((label+textbox (name label)
             (with-html
               (label name label)
               (textbox name
                        :id (string-downcase name)
                        :value (getf data (make-keyword name))
                        :disabledp disabledp
                        :style (getf styles (make-keyword name))))))
      (with-html
        (:div :id "cheque-data-form" :class "data-form"
              (label+textbox 'bank "Τράπεζα")
              (label+textbox 'company "Εταιρία")
              (label+textbox 'due-date "Ημερομηνία πληρωμής")
              (label+textbox 'amount "Ποσό")
              (:div :class "grid_3 alpha cheque-data-form-title"
                    (label 'status "Κατάσταση")
                    (dropdown 'status
                              (with-db ()
                                (query (:select 'description 'id
                                                :from 'cheque-status)))
                              :selected (or (getf data :status) *default-cheque-status*)
                              :disabledp disabledp)))
        (:div :class "data-form-buttons grid_9"
              (if disabledp
                  (cancel-button (cheque cheque-kind :id id :search search)
                                 "Επιστροφή στον Κατάλογο Επιταγών")
                  (progn
                    (ok-button (if (eql op 'update) "Ανανέωση" "Δημιουργία"))
                    (cancel-button (cheque cheque-kind :id id :search search) "Άκυρο"))))))))
