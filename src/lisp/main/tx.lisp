(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass tx-family (family-mixin)
  ()
  (:default-initargs
   :parameter-groups '(:system (tx-id)
                       :payload (tx-date description company amount
                                 non-chq-debit-acc non-chq-credit-acc)
                       :filter  (search since until))))

(defclass tx-page (auth-dynamic-page tx-family)
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
        :amount-overflow
        "Το ποσό της συναλλαγής είναι δεν πρέπει να ξεπερνά το 9,999,999.99"
        :parse-error
        "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες"))
      (non-chq-debit-acc
       (:account-title-null
        "Ο λογαριασμός χρέωσης είναι κενός"
        :account-title-unknown
        "Λάθος λογαριασμός χρέωσης: Δεν έχει καταχωρηθεί λογαριασμός με αυτό το όνομα"
        :unknown-implicit-temtx
        "Δεν υπάρχει πρότυπη συναλλαγή που αντιστοιχεί σε αυτούς τους λογαριασμούς χρέωσης/πίστωσης"))
      (non-chq-credit-acc
       (:account-title-null
        "Ο λογαριασμός πίστωσης είναι κενός"
        :account-title-unknown
        "Λάθος λογαριασμός πίστωσης: Δεν έχει καταχωρηθεί λογαριασμός με αυτό το όνομα"
        :unknown-implicit-temtx ""))
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

(defun check-temtx-existence (debit-account credit-account)
  (let ((temtx-exists-p (with-db ()
                          (query (:select (:not (:is-null (:get-temtx (account-id debit-account)
                                                                      (account-id credit-account)))))
                                 :single!))))
    (if temtx-exists-p nil :unknown-implicit-temtx)))

(defun tx-referenced-p (tx-id)
  (referenced-by tx-id 'cheque-event 'tx-id))

(defun chk-tx-id (tx-id)
  (if (tx-id-exists-p tx-id)
      nil
      :tx-id-unknown))

(defun chk-tx-id/ref (tx-id)
  (cond ((chk-tx-id tx-id))
        ((tx-referenced-p tx-id) :tx-referenced)))


;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun tx-top-actions (op)
  (top-actions-area
   (make-instance 'scrooge-menu
                  :spec (make-menu-spec `(:create (,(family-url 'tx/create :filter) "Νέα Συναλλαγή")))
                  :css-class "hmenu"
                  :disabled (list op))
   (searchbox (family-url-fn 'tx)
              (family-url-fn 'tx :system)
              (family-params 'tx :filter)
              "ac-company")))



;;; ----------------------------------------------------------------------
;;; TX form
;;; ----------------------------------------------------------------------

(defclass tx-form (crud-form/plist)
  ())

(defmethod get-record ((form tx-form))
  (if-let (tx-id (key form))
    (query (:select 'tx.id 'tx-date
             (:as 'company.title 'company)
             'description
             (:as 'non-chq-debit-acc.title 'non-chq-debit-acc)
             (:as 'non-chq-credit-acc.title 'non-chq-credit-acc)
             'tx.debit-acc-id
             'tx.credit-acc-id
             'amount 'company-id
             :from 'tx
             :left-join 'company
             :on (:= 'tx.company-id 'company.id)
             :left-join (:as 'account 'non-chq-debit-acc)
             :on (:= 'non-chq-debit-acc.id 'debit-acc-id)
             :left-join (:as 'account 'non-chq-credit-acc)
             :on (:= 'non-chq-credit-acc.id 'credit-acc-id)
             :where (:= 'tx.id tx-id))
           :plist)
    nil))



;;; ----------------------------------------------------------------------
;;; TX table
;;; ----------------------------------------------------------------------

;;; table

(defclass tx-table (scrooge-table)
  ((header-labels  :initform '("" "Ημερομηνία" "Εταιρία" "Περιγραφή"
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

(defmethod actions ((tbl tx-table) &key)
  (let* ((tx-id (selected-key tbl))
         (filter (filter tbl))
         (hrefs (if (and tx-id (not (tx-referenced-p tx-id)))
                    (list :update (apply #'tx/update :tx-id tx-id filter)
                          :delete (apply #'tx/delete :tx-id tx-id filter))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))

(defmethod filters ((tbl tx-table))
  (filter-area (datebox #'tx (filter tbl))))


;;; rows

(defclass tx-row (scrooge-row/plist)
  ())

(defmethod selector ((row tx-row) selected-p)
  (simple-selector row selected-p #'tx :tx-id))

(defmethod controls ((row tx-row) controls-p)
  (simple-controls row controls-p #'tx :tx-id))

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
                 (:a :href (company/details :company-id (getf record :company-id))
                   (str (getf record :company)))))
           (append (mapcar (lambda (name)
                             (make-instance 'textbox
                                            :name name
                                            :value (getf record (make-keyword name))
                                            :disabled (not enabled-p)
                                            :css-class (if enabled-p (getf css-class name) nil)))
                           '(description non-chq-debit-acc non-chq-credit-acc))
                   (list (make-instance 'textbox
                                        :name 'amount
                                        :value (fmt-amount (getf record :amount))
                                        :disabled (not enabled-p)))))))


;;; paginator

(defclass tx-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg tx-paginator) start)
  (apply #'tx :start start (filter (table pg))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defpage tx-page tx ("tx")
    ((tx-id  integer chk-tx-id)
     (search string)
     (since  date)
     (until  date)
     (start  integer))
  (with-view-page
    (let* ((filter (params->filter))
           (tx-table (make-instance 'tx-table
                                    :op :catalogue
                                    :selected-key (val tx-id)
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
            (tx-top-actions :catalogue)
            (filters tx-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" "Κατάλογος")
                (actions tx-table)
                (display tx-table)))
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
  ;; post-validation - prevent update for unknown temtx of if tx is referenced
  (validate-parameters #'check-temtx-existence non-chq-debit-acc non-chq-credit-acc)
  (with-view-page
    (let* ((filter (params->filter))
           (tx-table (make-instance 'tx-table
                                    :op :create
                                    :filter filter)))
      (with-document ()
        (:head
          (:title "Συναλλαγές » Δημιουργία")
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header)
            (main-navbar 'tx)
            (tx-top-actions :create)
            (filters tx-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" "Δημιουργία")
                (actions tx-table)
                (notifications)
                (with-form (actions/tx/create :search (val search)
                                              :since (val since)
                                              :until (val until))
                  (display tx-table :payload (params->payload)))))
            (footer)))))))

(defpage tx-page actions/tx/create ("actions/tx/create"
                                    :request-type :post)
    ((search             string)
     (since              date)
     (until              date)
     (tx-date            date   chk-date)
     (description        string)
     (company            string chk-company-title)
     (amount             float  chk-amount)
     (non-chq-debit-acc  string chk-non-chq-acc-title)
     (non-chq-credit-acc string chk-non-chq-acc-title))
  ;; post-validation - prevent update for unknown temtx of if tx is referenced
  (validate-parameters #'check-temtx-existence non-chq-debit-acc non-chq-credit-acc)
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
      (see-other (apply #'tx :tx-id (tx-id new-tx) (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage tx-page tx/update ("tx/update")
    ((search             string)
     (tx-id              integer chk-tx-id             t)
     (since              date)
     (until              date)
     (tx-date            date    chk-date)
     (description        string)
     (company            string  chk-company-title)
     (amount             float   chk-amount)
     (non-chq-debit-acc  string  chk-non-chq-acc-title)
     (non-chq-credit-acc string  chk-non-chq-acc-title))
  ;; post-validation - prevent update for unknown temtx of if tx is referenced
  (validate-parameters #'check-temtx-existence non-chq-debit-acc non-chq-credit-acc)
  (validate-parameters #'tx-referenced-p tx-id)
  (with-view-page
    (let* ((filter (params->filter))
           (tx-table (make-instance 'tx-table
                                    :op :update
                                    :selected-key (val tx-id)
                                    :filter filter)))
      (with-document ()
        (:head
          (:title "Συναλλαγή » Επεξεργασία")
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header)
            (main-navbar 'tx)
            (tx-top-actions :update)
            (filters tx-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" "Επεξεργασία")
                (actions tx-table)
                (notifications)
                (with-form (actions/tx/update :tx-id (val tx-id)
                                              :search (val search)
                                              :since (val since)
                                              :until (val until))
                  (display tx-table :payload (params->payload)))))
            (footer)))))))

(defpage tx-page actions/tx/update ("actions/tx/update"
                                    :request-type :post)
    ((search             string)
     (since              date)
     (until              date)
     (tx-id              integer chk-tx-id             t)
     (tx-date            date    chk-date)
     (description        string)
     (company            string  chk-company-title)
     (amount             float   chk-amount)
     (non-chq-debit-acc  string  chk-non-chq-acc-title)
     (non-chq-credit-acc string  chk-non-chq-acc-title))
  ;; post-validation - prevent update for unknown temtx of if tx is referenced
  (validate-parameters #'check-temtx-existence non-chq-debit-acc non-chq-credit-acc)
  (validate-parameters #'tx-referenced-p tx-id)
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
                        :where (:= 'id (val tx-id))))
      (see-other (apply #'tx :tx-id (val tx-id) (params->filter))))))



;;; -----------------------------------------------------------------------
;;; DELETE
;;; -----------------------------------------------------------------------

(defpage tx-page tx/delete ("tx/delete")
    ((tx-id  integer chk-tx-id/ref t)
     (since  date)
     (until  date)
     (search string))
  ;; post validation - prevent delete if automatically created
  (with-db ()
    (validate-parameters #'tx-referenced-p tx-id))
  (with-view-page
    (let* ((filter (params->filter))
           (tx-table (make-instance 'tx-table
                                    :op :delete
                                    :selected-key (val tx-id)
                                    :filter filter)))
      (with-document ()
        (:head
          (:title "Συναλλαγή » Διαγραφή")
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header)
            (main-navbar 'tx)
            (tx-top-actions :delete)
            (filters tx-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" "Διαγραφή")
                (actions tx-table)
                (with-form (actions/tx/delete :tx-id (val tx-id)
                                              :search (val search)
                                              :since (val since)
                                              :until (val until))
                  (display tx-table))))
            (footer)))))))

(defpage tx-page actions/tx/delete ("actions/tx/delete"
                                    :request-type :post)
    ((tx-id  integer chk-tx-id/ref t)
     (since  date)
     (until  date)
     (search string))
  (with-controller-page (tx/delete)
    (delete-dao (get-dao 'tx (val tx-id)))
    (see-other (apply #'tx (params->filter)))))
