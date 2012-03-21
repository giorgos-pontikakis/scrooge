(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass tx-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id))
   (payload-parameter-names
    :allocation :class
    :initform '(tx-date description company amount non-chq-debit-acc non-chq-credit-acc))
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
        :amount-overflow
        "Το ποσό της συναλλαγής είναι δεν πρέπει να ξεπερνά το 9,999,999.99"
        :parse-error
        "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες"))
      (non-chq-debit-acc
       (:account-title-null
        "Ο λογαριασμός χρέωσης είναι κενός"
        :account-title-unknown
        "Λάθος λογαριασμός χρέωσης: Δεν έχει καταχωρηθεί λογαριασμός με αυτό το όνομα"))
      (non-chq-credit-acc
       (:account-title-null
        "Ο λογαριασμός πίστωσης είναι κενός"
        :account-title-unknown
        "Λάθος λογαριασμός πίστωσης: Δεν έχει καταχωρηθεί λογαριασμός με αυτό το όνομα"))
      (tx-date
       (:date-null
        "Η ημερομηνία της συναλλαγής είναι κενή"
        :parse-error
        "Η ημερομηνία της συναλλαγής είναι άκυρη"))))))



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(define-existence-predicate tx-id-exists-p tx id)
(define-existence-predicate* tx-description-exists-p tx description id)

(defun chk-tx-id (id)
  (if (tx-id-exists-p id)
      nil
      :tx-id-unknown))



;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun auto-tx-p (tx-id)
  (if tx-id
      (query (:select 'tx-id
              :from 'cheque-event
              :where (:= 'tx-id tx-id)))
      nil))

(defun tx-top-actions (id filter)
  (top-actions
   (make-instance 'menu
                  :spec `((create ,(html ()
                                     (:a :href (apply #'tx/create filter)
                                         (:img :src "/scrooge/img/add.png")
                                         (str "Νέα Συναλλαγή")))))
                  :css-class "hmenu"
                  :disabled nil)
   (searchbox #'tx
              #'(lambda (&rest args)
                  (apply #'tx :id id args))
              filter
              "ac-company")))

(defun tx-disabled-actions (op id auto-p)
  (ecase op
    (:catalogue (if (or (not id) auto-p)
                    '()
                    '(:update :delete)))
    (:create '())
    (:update '())
    (:delete '())))

(defun tx-actions (op id filter)
  (let ((auto-p (if id (auto-tx-p id) nil)))
    (actions-menu (make-menu-spec
                   (action-anchors/crud (if auto-p
                                            nil
                                            (apply #'tx/update :id id filter))
                                        (if auto-p
                                            nil
                                            (apply #'tx/delete :id id filter))))
                  (tx-disabled-actions op id auto-p))))

(defun tx-filters (filter)
  (filter-area (datebox #'tx filter)))



;;; ----------------------------------------------------------------------
;;; TX table
;;; ----------------------------------------------------------------------

;;; table

(defclass tx-table (scrooge-table)
  ((header-labels  :initform '("" "Ημ/νία" "Εταιρία" "Περιγραφή"
                               "Λ. Χρέωσης" "Λ. Πίστωσης" "Ποσό" "" ""))
   (paginator      :initform (make-instance 'tx-paginator
                                            :id "tx-paginator"
                                            :css-class "paginator")))
  (:default-initargs :item-class 'tx-row :id "tx-table"))

(defmethod get-records ((table tx-table))
  (let* ((search (getf (filter table) :search))
         (since (getf (filter table) :since))
         (until (getf (filter table) :until))
         (base-query `(:select tx.id
                               (:as company.title company)
                               (:as company.id company-id)
                               (:as debit-acc.title non-chq-debit-acc)
                               (:as credit-acc.title non-chq-credit-acc)
                               tx-date
                               description
                               amount
                       :from tx
                       :left-join company
                       :on (:= tx.company-id company.id)
                       :inner-join (:as account debit-acc)
                       :on (:= debit-acc.id debit-acc-id)
                       :inner-join (:as account credit-acc)
                       :on (:= credit-acc.id credit-acc-id)))
         (where nil))
    (when search
      (push `(:or (:ilike description ,(ilike search))
                  (:ilike company.title ,(ilike search))
                  (:ilike debit-acc.title ,(ilike search))
                  (:ilike credit-acc.title ,(ilike search)))
            where))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since tx-date) where))
    (when (and until (not (eql until :null)))
      (push `(:<= tx-date ,until) where))
    (let ((sql `(:order-by (,@base-query :where (:and t ,@where))
                           (:desc tx-date) non-chq-debit-acc non-chq-credit-acc company description)))
      (query (sql-compile sql)
             :plists))))


;;; rows

(defclass tx-row (scrooge-row/plist)
  ())

(defmethod selector ((row tx-row) selected-p)
  (simple-selector row selected-p #'tx))

(defmethod controls ((row tx-row) controls-p)
  (simple-controls row controls-p #'tx))

(defmethod payload ((row tx-row) enabled-p)
  (let ((record (record row))
        (css-class '(tx-date "datepicker"
                     company "ac-company"
                     non-chq-debit-acc "ac-non-chq-account"
                     non-chq-credit-acc "ac-non-chq-account")))
    (list* (make-instance 'textbox
                          :name 'tx-date
                          :value (or (getf record :tx-date) (today))
                          :disabled (not enabled-p)
                          :css-class (if enabled-p (getf css-class 'tx-date) nil))
           (if (member (op (collection row)) '(:create :update))
               (make-instance 'textbox
                              :name 'company
                              :value (getf record :company)
                              :disabled (not enabled-p)
                              :css-class (if enabled-p (getf css-class 'company) nil))
               (html ()
                 (:a :href (company/details :id (getf record :company-id))
                     (str (getf record :company)))))
           (mapcar (lambda (name)
                     (make-instance 'textbox
                                    :name name
                                    :value (getf record (make-keyword name))
                                    :disabled (not enabled-p)
                                    :css-class (if enabled-p (getf css-class name) nil)))
                   '(description non-chq-debit-acc non-chq-credit-acc amount)))))


;;; paginator

(defclass tx-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg tx-paginator) start)
  (apply #'tx :start start (filter (table pg))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defpage tx-page tx ("tx")
    ((id     integer chk-tx-id)
     (search string)
     (since  date)
     (until  date)
     (start  integer))
  (with-view-page
    (let* ((op :catalogue)
           (filter (params->filter))
           (tx-table (make-instance 'tx-table
                                    :op op
                                    :filter filter
                                    :start-index (val start))))
      (with-document ()
        (:head
         (:title "Συναλλαγές")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'tx)
               (tx-top-actions (val id) filter)
               (tx-filters filter)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" "Κατάλογος")
                           (tx-actions op (val id) filter)
                           (display tx-table
                                    :key (val id))))
               (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage tx-page tx/create ("tx/create")
    ((search             string)
     (since              date)
     (until              date)
     (tx-date            date   chk-date)
     (description        string)
     (company            string chk-company-title)
     (amount             float  chk-amount)
     (non-chq-debit-acc  string chk-non-chq-acc-title)
     (non-chq-credit-acc string chk-non-chq-acc-title))
  (with-view-page
    (let* ((op :create)
           (filter (params->filter))
           (tx-table (make-instance 'tx-table
                                    :op op
                                    :filter filter)))
      (with-document ()
        (:head
         (:title "Συναλλαγές » Δημιουργία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'tx)
               (tx-top-actions nil filter)
               (tx-filters filter)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" "Δημιουργία")
                           (tx-actions op nil filter)
                           (notifications)
                           (with-form (actions/tx/create :search (val search)
                                                         :since (val since)
                                                         :until (val until))
                             (display tx-table :payload (params->payload)))))
               (footer)))))))

(defpage tx-page actions/tx/create ("actions/tx/create"
                                    :request-type :post)
    ((search         string)
     (since          date)
     (until          date)
     (tx-date        date    chk-date)
     (description    string)
     (company        string  chk-company-title)
     (amount         float   chk-amount)
     (non-chq-debit-acc  string  chk-non-chq-acc-title)
     (non-chq-credit-acc string  chk-non-chq-acc-title))
  (with-controller-page (tx/create)
    (let* ((company-id (company-id (val company)))
           (debit-acc-id (account-id (val non-chq-debit-acc)))
           (credit-acc-id (account-id (val non-chq-credit-acc)))
           (new-tx (make-instance 'tx
                                  :tx-date (val tx-date)
                                  :description (val description)
                                  :company-id company-id
                                  :amount (val amount)
                                  :credit-acc-id credit-acc-id
                                  :debit-acc-id debit-acc-id)))
      (insert-dao new-tx)
      (see-other (apply #'tx :id (tx-id new-tx) (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage tx-page tx/update ("tx/update")
    ((search             string)
     (id                 integer chk-tx-id             t)
     (since              date)
     (until              date)
     (tx-date            date    chk-date)
     (description        string)
     (company            string  chk-company-title)
     (amount             float   chk-amount)
     (non-chq-debit-acc  string  chk-non-chq-acc-title)
     (non-chq-credit-acc string  chk-non-chq-acc-title))
  ;; post validation - prevent update if automatically created
  (with-db ()
    (validate-parameters #'auto-tx-p id))
  (with-view-page
    (let* ((op :update)
           (filter (params->filter))
           (tx-table (make-instance 'tx-table
                                    :op op
                                    :filter filter)))
      (with-document ()
        (:head
         (:title "Συναλλαγή » Επεξεργασία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'tx)
               (tx-top-actions id filter)
               (tx-filters filter)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" "Επεξεργασία")
                           (tx-actions op (val id) filter)
                           (notifications)
                           (with-form (actions/tx/update :id (val id)
                                                         :search (val search)
                                                         :since (val since)
                                                         :until (val until))
                             (display tx-table :key (val id)
                                               :payload (params->payload)))))
               (footer)))))))

(defpage tx-page actions/tx/update ("actions/tx/update"
                                    :request-type :post)
    ((search         string)
     (since          date)
     (until          date)
     (id             integer chk-tx-id t)
     (tx-date        date    chk-date)
     (description    string)
     (company        string  chk-company-title)
     (amount         float   chk-amount)
     (non-chq-debit-acc  string  chk-non-chq-acc-title)
     (non-chq-credit-acc string  chk-non-chq-acc-title))
  (with-controller-page (tx/update)
    (let ((company-id (company-id (val company)))
          (debit-acc-id (account-id (val non-chq-debit-acc)))
          (credit-acc-id (account-id (val non-chq-credit-acc))))
      (execute (:update 'tx :set
                        'tx-date (val tx-date)
                        'description (val description)
                        'company-id company-id
                        'amount (val amount)
                        'debit-acc-id debit-acc-id
                        'credit-acc-id credit-acc-id
                        :where (:= 'id (val id))))
      (see-other (apply #'tx :id (val id) (params->filter))))))



;;; -----------------------------------------------------------------------
;;; DELETE
;;; -----------------------------------------------------------------------

(defpage tx-page tx/delete ("tx/delete")
    ((id     integer chk-tx-id t)
     (since  date)
     (until  date)
     (search string))
  ;; post validation - prevent delete if automatically created
  (with-db ()
    (validate-parameters #'auto-tx-p id))
  (with-view-page
    (let* ((op :delete)
           (filter (params->filter))
           (tx-table (make-instance 'tx-table
                                    :op op
                                    :filter filter)))
      (with-document ()
        (:head
         (:title "Συναλλαγή » Διαγραφή")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'tx)
               (tx-top-actions (val id) filter)
               (tx-filters filter)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" "Διαγραφή")
                           (tx-actions op (val id) filter)
                           (with-form (actions/tx/delete :id (val id)
                                                         :search (val search)
                                                         :since (val since)
                                                         :until (val until))
                             (display tx-table :key (val id)))))
               (footer)))))))

(defpage tx-page actions/tx/delete ("actions/tx/delete"
                                    :request-type :post)
    ((id     integer chk-tx-id t)
     (since  date)
     (until  date)
     (search string))
  (with-controller-page (tx/delete)
    (delete-dao (get-dao 'tx (val id)))
    (see-other (apply #'tx (params->filter)))))




;;; ----------------------------------------------------------------------
;;; TX form
;;; ----------------------------------------------------------------------

(defmethod get-record ((type (eql 'tx)) id)
  (declare (ignore type))
  (query (:select 'tx.id 'tx-date
                  (:as 'company.title 'company)
                  'description
                  (:as 'non-chq-debit-acc.title 'non-chq-debit-acc)
                  (:as 'non-chq-credit-acc.title 'non-chq-credit-acc)
                  'tx.debit-acc-id
                  'tx.credit-acc-id
                  'amount
                  :from 'tx
                  :left-join 'company
                  :on (:= 'tx.company-id 'company.id)
                  :left-join (:as 'account 'non-chq-debit-acc)
                  :on (:= 'non-chq-debit-acc.id 'debit-acc-id)
                  :left-join (:as 'account 'non-chq-credit-acc)
                  :on (:= 'non-chq-credit-acc.id 'credit-acc-id)
                  :where (:= 'tx.id id))
         :plist))
