(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass tx-family (family-mixin)
  ()
  (:default-initargs
   :parameter-groups '(:system (tx-id)
                       :payload (tx-date description company amount
                                 non-chq-debit-account non-chq-credit-account)
                       :filter (search since until))))

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
      (non-chq-debit-account
       (:account-title-null
        "Ο λογαριασμός χρέωσης είναι κενός"
        :account-title-unknown
        "Λάθος λογαριασμός χρέωσης: Δεν έχει καταχωρηθεί λογαριασμός με αυτό το όνομα"
        :unknown-temtx-for-account-pair
        "Δεν υπάρχει πρότυπη συναλλαγή που αντιστοιχεί σε αυτούς τους λογαριασμούς χρέωσης/πίστωσης"))
      (non-chq-credit-account
       (:account-title-null
        "Ο λογαριασμός πίστωσης είναι κενός"
        :account-title-unknown
        "Λάθος λογαριασμός πίστωσης: Δεν έχει καταχωρηθεί λογαριασμός με αυτό το όνομα"
        :unknown-temtx-for-account-pair ""))
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

(defun chk-temtx-existence (debit-account credit-account)
  (let ((temtx-exists-p
          (with-db ()
            (query (:select (:not (:is-null (:get-temtx (account-id (val debit-account))
                                                        (account-id (val credit-account))))))
                   :single!))))
    (if temtx-exists-p nil :unknown-temtx-for-account-pair)))

(defun tx-referenced-p (tx-id)
  (referenced-by tx-id 'cheque-event 'tx-id))

(defun chk-tx-references (tx-id)
  (if (tx-referenced-p tx-id) :tx-referenced nil))

(defun chk-tx-id (tx-id)
  (if (tx-id-exists-p tx-id)
      nil
      :tx-id-unknown))

(defun chk-tx-id/ref (tx-id)
  (cond ((chk-tx-id tx-id))
        ((tx-referenced-p tx-id) :tx-referenced)))

(defun tx-project-constraints-chker (account-id)
  #'(lambda (project-id)
      (if (eql account-id (account-id 'project-account))
          (cond ((not (suppliedp project-id))
                 :project-id-not-supplied)
                ((eql (val project-id) :null)
                 :project-id-null)
                ((and (val project-id)
                      (not (project-id-exists-p (val project-id))))
                 :project-id-unknown))
          nil)))



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
                    (:as 'non-chq-debit-account.title 'non-chq-debit-account)
                    (:as 'non-chq-credit-account.title 'non-chq-credit-account)
                    'tx.debit-account-id
                    'tx.credit-account-id
                    'amount 'company-id 'temtx-id
            :from 'tx
            :left-join 'company
            :on (:= 'tx.company-id 'company.id)
            :left-join (:as 'account 'non-chq-debit-account)
            :on (:= 'non-chq-debit-account.id 'debit-account-id)
            :left-join (:as 'account 'non-chq-credit-account)
            :on (:= 'non-chq-credit-account.id 'credit-account-id)
            :where (:= 'tx.id tx-id))
           :plist)
    nil))



;;; ----------------------------------------------------------------------
;;; TX table
;;; ----------------------------------------------------------------------

;;; table

(defclass tx-table (scrooge-crud-table/plist)
  ((header-labels :initform '("" "Ημερομηνία" "Εταιρία" "Περιγραφή"
                              "Λ. Χρέωσης" "Λ. Πίστωσης" "Ποσό" "" ""))
   (paginator :initform (make-instance 'tx-paginator
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
                               (:as debit-acc.title non-chq-debit-account)
                               (:as credit-acc.title non-chq-credit-account)
                               (:as debit-acc.id non-chq-debit-account-id)
                               (:as credit-acc.id non-chq-credit-account-id)
                               tx-date
                               description
                               amount
                               :from tx
                               :left-join company
                               :on (:= tx.company-id company.id)
                               :inner-join (:as account debit-acc)
                               :on (:= debit-acc.id debit-account-id)
                               :inner-join (:as account credit-acc)
                               :on (:= credit-acc.id credit-account-id)))
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
                           (:desc tx-date) non-chq-debit-account non-chq-credit-account
                           company description)))
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

(defclass tx-row (scrooge-row)
  ())

(defmethod selector ((row tx-row) selected-p)
  (simple-selector row selected-p #'tx :tx-id))

(defmethod controls ((row tx-row) controls-p)
  (simple-controls row controls-p #'tx :tx-id))

(defmethod payload ((row tx-row) enabled-p)
  (let ((record (record row)))
    (mapcar (textbox-maker record enabled-p)
            `((tx-date :css-class ,(if enabled-p "datepicker" nil))
              (company :css-class ,(if enabled-p "ac-company" nil)
                       :href ,(company/tx :company-id (getf record :company-id) :tx-id (key row)))
              description
              (non-chq-debit-account :href ,(account/tx :account-id (getf record
                                                                          :non-chq-debit-account-id)
                                                        :tx-id (getf record :id))
                                     :css-class ,(if enabled-p "ac-non-chq-account" nil))
              (non-chq-credit-account :href ,(account/tx :account-id (getf record
                                                                           :non-chq-credit-account-id)
                                                         :tx-id (getf record :id))
                                      :css-class ,(if enabled-p "ac-non-chq-account" nil))
              (amount :format-fn ,#'fmt-amount)))))


;;; paginator

(defclass tx-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg tx-paginator) start)
  (apply #'tx :start start (filter (table pg))))



;;; ----------------------------------------------------------------------
;;; Utilities
;;; ----------------------------------------------------------------------

(defun tx-role (record)
  (if (or (member (getf record :debit-account-id) *expense-accounts*)
          (member (getf record :credit-account-id) *expense-accounts*))
      "supplier"
      "customer"))



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
          (advanced-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'advanced)
            (advanced-navbar 'tx)
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
    ((search                 string)
     (since                  date)
     (until                  date)
     (tx-date                date   chk-date)
     (description            string)
     (company                string chk-company-title)
     (amount                 float  chk-amount)
     (non-chq-debit-account  string chk-non-chq-account-title)
     (non-chq-credit-account string chk-non-chq-account-title))
  ;; post-validation - prevent update for non-existent temtx
  (validate-parameters #'chk-temtx-existence non-chq-debit-account non-chq-credit-account)
  (with-view-page
    (let* ((filter (params->filter))
           (tx-table (make-instance 'tx-table
                                    :op :create
                                    :filter filter)))
      (with-document ()
        (:head
          (:title "Συναλλαγές » Δημιουργία")
          (advanced-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'advanced)
            (advanced-navbar 'tx)
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
    ((search                 string)
     (since                  date)
     (until                  date)
     (tx-date                date   chk-date)
     (description            string)
     (company                string chk-company-title)
     (amount                 float  chk-amount)
     (non-chq-debit-account  string chk-non-chq-account-title)
     (non-chq-credit-account string chk-non-chq-account-title))
  ;; post-validation - prevent update for non-existent temtx
  (validate-parameters #'chk-temtx-existence non-chq-debit-account non-chq-credit-account)
  (with-controller-page (tx/create)
    (let* ((company-id (company-id (val company)))
           (debit-account-id (account-id (val non-chq-debit-account)))
           (credit-account-id (account-id (val non-chq-credit-account)))
           (new-tx (make-instance 'tx
                                  :tx-date (val tx-date)
                                  :description (val description)
                                  :company-id company-id
                                  :amount (val amount)
                                  :credit-account-id credit-account-id
                                  :debit-account-id debit-account-id)))
      (insert-dao new-tx)
      (see-other (apply #'tx :tx-id (tx-id new-tx) (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage tx-page tx/update ("tx/update")
    ((search                 string)
     (tx-id                  integer chk-tx-id                 t)
     (since                  date)
     (until                  date)
     (tx-date                date    chk-date)
     (description            string)
     (company                string  chk-company-title)
     (amount                 float   chk-amount)
     (non-chq-debit-account  string  chk-non-chq-account-title)
     (non-chq-credit-account string  chk-non-chq-account-title))
  ;; post-validation - prevent update for non-existent temtx or if tx is referenced
  (validate-parameters #'chk-temtx-existence non-chq-debit-account non-chq-credit-account)
  (validate-parameters #'chk-tx-references tx-id)
  (with-view-page
    (let* ((filter (params->filter))
           (tx-table (make-instance 'tx-table
                                    :op :update
                                    :selected-key (val tx-id)
                                    :filter filter)))
      (with-document ()
        (:head
          (:title "Συναλλαγή » Επεξεργασία")
          (advanced-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'advanced)
            (advanced-navbar 'tx)
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
    ((search                 string)
     (since                  date)
     (until                  date)
     (tx-id                  integer chk-tx-id                 t)
     (tx-date                date    chk-date)
     (description            string)
     (company                string  chk-company-title)
     (amount                 float   chk-amount)
     (non-chq-debit-account  string  chk-non-chq-account-title)
     (non-chq-credit-account string  chk-non-chq-account-title))
  ;; post-validation - prevent update for non-existent temtx or if tx is referenced
  (validate-parameters #'chk-temtx-existence non-chq-debit-account non-chq-credit-account)
  (validate-parameters #'chk-tx-references tx-id)
  (with-controller-page (tx/update)
    (let ((company-id (company-id (val company)))
          (debit-account-id (account-id (val non-chq-debit-account)))
          (credit-account-id (account-id (val non-chq-credit-account))))
      (execute (:update 'tx :set
                        'tx-date (val tx-date)
                        'description (val description)
                        'company-id company-id
                        'amount (val amount)
                        'debit-account-id debit-account-id
                        'credit-account-id credit-account-id
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
    (validate-parameters #'chk-tx-references tx-id))
  (with-view-page
    (let* ((filter (params->filter))
           (tx-table (make-instance 'tx-table
                                    :op :delete
                                    :selected-key (val tx-id)
                                    :filter filter)))
      (with-document ()
        (:head
          (:title "Συναλλαγή » Διαγραφή")
          (advanced-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'advanced)
            (advanced-navbar 'tx)
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
