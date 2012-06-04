(in-package :scrooge)


;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass company-tx-family (tx-family)
  ()
  (:default-initargs :parameter-groups '(:system (company-id tx-id start)
                                         :payload (tx-date description debit-amount credit-amount)
                                         :filter (search subset role since until))))

(defclass company-tx-page (auth-dynamic-page company-tx-family)
  ((messages
    :allocation :class
    :reader messages
    :initform
    '(((debit-amount credit-amount)
       (:empty-amount
        "Το ποσό της συναλλαγής είναι κενό"
        :non-positive-amount
        "Το ποσό της συναλλαγής δεν είναι θετικός αριθμός"
        :amount-overflow
        "Το ποσό της συναλλαγής είναι δεν πρέπει να ξεπερνά το 9,999,999.99"
        :parse-error
        "Το ποσό της συναλλαγής περιέχει άκυρους χαρακτήρες"))
      (tx-date
       (:date-null
        "Η ημερομηνία της συναλλαγής είναι κενή"
        :parse-error
        "Η ημερομηνία της συναλλαγής είναι άκυρη"))))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun company-tx-top-actions (op)
  (top-actions-area (make-instance 'scrooge-menu
                                   :spec (make-menu-spec
                                          `(:catalogue ,(family-url 'company :system :filter)
                                            :print ,(family-url 'company/tx/print :system :filter)))
                                   :css-class "hmenu"
                                   :disabled (case op
                                               (:create '(:create-company :create-cheque :print))
                                               (:update '(:print))))
                    (searchbox (family-url-fn 'actions/company/search)
                               (family-url-fn 'company :system)
                               (family-params 'company :filter)
                               "ac-company")))



;;; ------------------------------------------------------------
;;; Company tx table
;;; ------------------------------------------------------------

(defun cheque-row-p (row)
  (let ((due-date (getf row :due-date)))
    (if (eql due-date :null)
        nil
        due-date)))


;;;  Customer

(defun customer-debits ()
  `(:in tx.credit-acc-id (:set ,@*revenues-accounts*)))

(defun customer-cash-credits ()
  `(:= tx.debit-acc-id ,*cash-acc-id*))

(defun customer-contra-credits ()
  `(:in tx.debit-acc-id (:set ,@*revenues-accounts*)))

(defun customer-cheque-credits ()
  ;; subquery receives cheque-event.cheque-id from main query
  `(:and (:= tx.debit-acc-id ,*cheque-receivable-acc-id*)
         (:not (:exists (:select 1
                         :from (:as tx tx2)
                         :inner-join (:as cheque-event cheque-event2)
                         :on (:= cheque-event2.tx-id tx2.id)
                         :where (:and
                                 (:= cheque-event2.cheque-id
                                     cheque-event.cheque-id)
                                 (:= tx2.credit-acc-id
                                     ,*cheque-receivable-acc-id*)))))))

;;;  Supplier

(defun supplier-credits ()
  `(:in tx.debit-acc-id (:set ,@*expense-accounts*)))

(defun supplier-cash-debits ()
  `(:= tx.credit-acc-id ,*cash-acc-id*))

(defun supplier-contra-debits ()
  `(:in tx.credit-acc-id (:set ,@*expense-accounts*)))

(defun supplier-cheque-debits ()
  ;; subquery receives cheque-event.cheque-id from main query
  `(:and (:= tx.credit-acc-id ,*cheque-payable-acc-id*)
         (:not (:exists (:select 1
                         :from (:as tx tx2)
                         :inner-join (:as cheque-event cheque-event2)
                         :on (:= cheque-event2.tx-id tx2.id)
                         :where (:and
                                 (:= cheque-event2.cheque-id
                                     cheque-event.cheque-id)
                                 (:= tx2.debit-acc-id
                                     ,*cheque-payable-acc-id*)))))))

(defun company-debits (company-id roles &optional since until)
  (let ((base-query '(:select tx-date (:as tx.id id) tx.description
                      (:as tx.amount debit-amount) cheque.due-date cheque.state-id
                      :from tx
                      :left-join cheque-event
                      :on (:= cheque-event.tx-id tx.id)
                      :left-join cheque
                      :on (:= cheque.id cheque-event.cheque-id)))
        (where-base `(:= tx.company-id ,company-id))
        (where-tx '())
        (where-dates nil))
    (when (member :customer roles)
      (push (customer-debits) where-tx))
    (when (member :supplier roles)
      (push (supplier-cash-debits) where-tx)
      (push (supplier-contra-debits) where-tx)
      (push (supplier-cheque-debits) where-tx))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since tx-date) where-dates))
    (when (and until (not (eql until :null)))
      (push `(:<= tx-date ,until) where-dates))
    (let ((sql `(:order-by (,@base-query
                            :where (:and ,where-base
                                         (:or ,@where-tx)
                                         ,@where-dates))
                           'tx-date)))
      (query (sql-compile sql)
             :plists))))

(defun company-credits (company-id roles &optional since until)
  (let ((base-query '(:select tx-date (:as tx.id id) tx.description
                      (:as tx.amount credit-amount) cheque.due-date cheque.state-id
                      :from tx
                      :left-join cheque-event
                      :on (:= cheque-event.tx-id tx.id)
                      :left-join cheque
                      :on (:= cheque.id cheque-event.cheque-id)))
        (where-base `(:= tx.company-id ,company-id))
        (where-tx `())
        (where-dates nil))
    (when (member :customer roles)
      (push (customer-cash-credits) where-tx)
      (push (customer-contra-credits) where-tx)
      (push (customer-cheque-credits) where-tx))
    (when (member :supplier roles)
      (push (supplier-credits) where-tx))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since tx-date) where-dates))
    (when (and until (not (eql until :null)))
      (push `(:<= tx-date ,until) where-dates))
    (let ((sql `(:order-by (,@base-query
                            :where (:and ,where-base
                                         (:or ,@where-tx)
                                         ,@where-dates))
                           'tx-date)))
      (query (sql-compile sql)
             :plists))))

(defun company-debits/credits (company-id roles since until)
  (flet ((get-tx-date (row)
           (getf row :tx-date)))
    (let* ((sorted (stable-sort (nconc (company-debits company-id roles)
                                       (company-credits company-id roles))
                                #'local-time:timestamp<
                                :key #'get-tx-date))
           (truncated (remove-if (lambda (row)
                                   (or (and since
                                            (not (eql since :null))
                                            (timestamp< (get-tx-date row) since))
                                       (and until
                                            (not (eql until :null))
                                            (timestamp> (get-tx-date row) until))))
                                 sorted)))
      (let ((total 0)
            (debit-sum 0)
            (credit-sum 0))
        (dolist (row sorted)
          (let* ((debit (getf row :debit-amount 0))
                 (credit (getf row :credit-amount 0))
                 (delta (- debit credit)))
            (setf total (+ total delta))
            (setf debit-sum (+ debit-sum debit))
            (setf credit-sum (+ credit-sum credit))
            (when (cheque-row-p row)
              (setf (getf row :description)
                    (concatenate 'string
                                 (getf row :description)
                                 " (Λήξη: "
                                 (lisp->html (getf row :due-date))
                                 ")")))
            (nconc row (list :total total))))
        (values (nreverse truncated) debit-sum credit-sum total)))))


;;; table

(defclass company-tx-table (tx-table)
  ((header-labels :initform '("" "Ημερομηνία" "Περιγραφή" "Χρέωση" "Πίστωση" "Υπόλοιπο" ""))
   (paginator     :initarg :paginator)
   (company-id    :accessor company-id
                  :initarg :company-id))
  (:default-initargs :item-class 'company-tx-row
                     :id "company-tx-table"
                     :paginator (make-instance 'company-tx-paginator
                                               :id "company-tx-paginator"
                                               :css-class "paginator")))

(defmethod actions ((tbl company-tx-table) &key)
  (let* ((company-id (company-id tbl))
         (filter (filter tbl))
         (tx-id (selected-key tbl))
         (hrefs (if (and tx-id (not (auto-tx-p tx-id)))
                    (list :update (apply #'company/tx/update :company-id company-id
                                                             :tx-id tx-id
                                                             filter))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))

(defmethod filters ((tbl company-tx-table))
  (let* ((company-id (company-id tbl))
         (filter (filter tbl))
         (filter* (remove-from-plist filter :role))
         (filter-spec `((nil      ,(apply #'company/tx :company-id company-id filter*)
                                  "Μικτός ρόλος")
                        (customer  ,(apply #'company/tx :company-id company-id
                                                        :role "customer" filter*)
                                   "Πελάτης")
                        (supplier ,(apply #'company/tx :company-id company-id
                                                       :role "supplier" filter*)
                                  "Προμηθευτής"))))
    (secondary-filter-area (filter-navbar filter-spec
                                          :active (getf filter :role))
                           (datebox #'company/tx
                                    (list* :company-id company-id filter)))))


;;; rows

(defclass company-tx-row (scrooge-row/plist)
  ())

(defmethod selector ((row company-tx-row) selected-p)
  (let* ((table (collection row))
         (tx-id (key row))
         (company-id (company-id table))
         (filter (filter table))
         (start (page-start (paginator table) (index row) (start-index table))))
    (html ()
      (:a :href (if selected-p
                    (apply #'company/tx :company-id company-id
                                        :start start filter)
                    (apply #'company/tx :company-id company-id
                                        :tx-id tx-id filter))
          (selector-img selected-p)))))

(defmethod controls ((row company-tx-row) controls-p)
  (let ((tx-id (key row))
        (table (collection row)))
    (if controls-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (apply #'company/tx
                                          :company-id (company-id table)
                                          :tx-id tx-id
                                          (filter table))))
        (list nil nil))))

(defmethod payload ((row company-tx-row) enabled-p)
  (let ((record (record row)))
    (list (make-instance 'textbox
                         :name 'tx-date
                         :value (getf record :tx-date)
                         :disabled (not enabled-p)
                         :css-class (if enabled-p "datepicker" nil))
          (make-instance 'textbox
                         :name 'description
                         :value (getf record :description)
                         :disabled (not enabled-p))
          (make-instance 'textbox
                         :name 'debit-amount
                         :value (fmt-amount (getf record :debit-amount))
                         :disabled (or (not (getf record :debit-amount))
                                       (not enabled-p)))
          (make-instance 'textbox
                         :name 'credit-amount
                         :value (fmt-amount (getf record :credit-amount))
                         :disabled (or (not (getf record :credit-amount))
                                       (not enabled-p)))
          (make-instance 'textbox
                         :name 'total
                         :value (fmt-amount (getf record :total))
                         :disabled t))))


;;; paginator

(defclass company-tx-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg company-tx-paginator) start)
  (let ((table (table pg)))
    (apply #'company/tx :start start
                        :company-id (company-id table)
                        (filter table))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defpage company-tx-page company/tx ("company/tx")
    ((company-id integer chk-company-id t)
     (tx-id      integer chk-tx-id)
     (start      integer)
     (search     string)
     (subset     string)
     (role       string)
     (since      date)
     (until      date))
  (with-view-page
    (let ((filter (params->filter))
          (roles (if (val role)
                     (list (make-keyword (string-upcase (val role))))
                     (list :customer :supplier))))
      (multiple-value-bind (records debit-sum credit-sum total)
          (company-debits/credits (val company-id) roles (val since) (val until))
        (let ((company-tx-table (make-instance 'company-tx-table
                                               :records records
                                               :company-id (val company-id)
                                               :op :catalogue
                                               :selected-key (val tx-id)
                                               :filter filter
                                               :start-index (val start))))
          (with-document ()
            (:head
             (:title "Εταιρία » Λεπτομέρειες » Συναλλαγές")
             (main-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header)
                   (main-navbar 'company)
                   (company-tx-top-actions :catalogue)
                   (company-tabs
                    (val company-id) filter 'tx
                    (html ()
                      (filters company-tx-table)
                      (:div :id "company-tx-window" :class "window"
                            (:div :class "title" "Συναλλαγές")
                            (actions company-tx-table)
                            (display company-tx-table)
                            (:h4 "Σύνολο Χρεώσεων: " (fmt "~9,2F" debit-sum))
                            (:h4 "Σύνολο Πιστώσεων: " (fmt "~9,2F" credit-sum))
                            (:h4 "Γενικό Σύνολο: " (fmt "~9,2F" total)))))
                   (footer)))))))))

(defpage company-tx-page company/tx/print ("company/tx/print")
    ((company-id integer chk-company-id t)
     (tx-id      integer chk-tx-id)
     (search     string)
     (subset     string)
     (role       string)
     (since      date)
     (until      date))
  (with-view-page
    (let ((filter (params->filter))
          (system (params->plist #'val-or-raw (list company-id tx-id)))
          (misc  (params->plist #'val-or-raw (list since until)))
          (roles (if (val role)
                     (list (make-keyword (string-upcase (val role))))
                     (list :customer :supplier))))
      (multiple-value-bind (records debit-sum credit-sum)
          (company-debits/credits (val company-id) roles (val since) (val until))
        (with-document ()
          (:head
           (:title "Εταιρία » Λεπτομέρειες » Συναλλαγές » Εκτύπωση")
           (print-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (:div :class "grid_12"
                       (:a :id "back"
                           :href (apply #'company/tx (append system filter misc))
                           "« Επιστροφή")
                       (:div :id "company-tx-window" :class "window"
                             (:div :class "title"
                                   (:h3 (str (title (get-dao 'company (val company-id)))))
                                   (display (datebox #'company/tx/print
                                                     (append system filter misc))))
                             (display (make-instance 'company-tx-table
                                                     :records records
                                                     :company-id (val company-id)
                                                     :op :details
                                                     :filter (append system filter misc)
                                                     :paginator nil))
                             (:h4 "Σύνολο χρεώσεων: " (fmt "~9,2F" debit-sum))
                             (:h4 "Σύνολο πιστώσεων: " (fmt "~9,2F" credit-sum))
                             (:h4 "Γενικό Σύνολο: " (fmt "~9,2F" (- debit-sum
                                                                    credit-sum))))))))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage company-tx-page company/tx/update ("company/tx/update")
    ((company-id    integer chk-company-id t)
     (tx-id         integer chk-tx-id)
     (tx-date       date)
     (description   string)
     (debit-amount  float   chk-amount)
     (credit-amount float   chk-amount)
     (search        string)
     (subset        string)
     (role          string)
     (since         date)
     (until         date))
  (with-view-page
    (let ((filter (params->filter))
          (roles (if (val role)
                     (list (make-keyword (string-upcase (val role))))
                     (list :customer :supplier))))
      (multiple-value-bind (records debit-sum credit-sum total)
          (company-debits/credits (val company-id) roles (val since) (val until))
        (let ((company-tx-table (make-instance 'company-tx-table
                                               :records records
                                               :company-id (val company-id)
                                               :op :update
                                               :selected-key (val tx-id)
                                               :filter filter)))
          (with-document ()
            (:head
             (:title "Εταιρία » Λεπτομέρειες » Συναλλαγές » Επεξεργασία")
             (main-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header)
                   (main-navbar 'company)
                   (company-tx-top-actions :update)
                   (company-tabs
                    (val company-id) filter 'tx
                    (html ()
                      (filters company-tx-table)
                      (:div :id "company-tx-window" :class "window"
                            (:div :class "title" "Συναλλαγές")
                            (actions company-tx-table)
                            (notifications)
                            (with-form
                                (actions/company/tx/update :company-id (val company-id)
                                                           :tx-id (val tx-id)
                                                           :search (val search)
                                                           :subset (val subset)
                                                           :since (val since)
                                                           :until (val until)
                                                           :role (val role))
                              (display company-tx-table :payload (params->payload)))
                            (:h4 "Σύνολο Χρεώσεων: " (fmt "~9,2F" debit-sum))
                            (:h4 "Σύνολο Πιστώσεων: " (fmt "~9,2F" credit-sum))
                            (:h4 "Γενικό Σύνολο: " (fmt "~9,2F" total)))))
                   (footer)))))))))

(defpage company-tx-page actions/company/tx/update
    ("actions/company/tx/update" :request-type :post)
    ((company-id    integer chk-company-id t)
     (tx-id         integer chk-tx-id)
     (tx-date       date)
     (description   string)
     (debit-amount  float   chk-amount)
     (credit-amount float   chk-amount)
     (search        string)
     (subset        string)
     (role          string)
     (since         date)
     (until         date))
  (with-controller-page (company/tx/update)
    (execute (:update 'tx :set
                      'tx-date (val tx-date)
                      'description (val description)
                      'company-id (val company-id)
                      'amount (or (val debit-amount)
                                  (val credit-amount))
                      :where (:= 'id (val tx-id))))
    (see-other (apply #'company/tx :company-id (val company-id)
                                   :tx-id (val tx-id)
                                   (params->filter)))))
